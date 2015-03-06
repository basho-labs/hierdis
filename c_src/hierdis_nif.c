// (The MIT License)

// Copyright (c) 2013 Nathan Aschbacher

// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// 'Software'), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#include "hierdis_nif.h"

static ERL_NIF_TERM hierdis_make_error(ErlNifEnv* env, int code, const char* reason)
{
    ERL_NIF_TERM atom;

    switch(code) 
    {
        case REDIS_REPLY_ERROR:
            atom = ATOM_REDIS_REPLY_ERROR;
            break;
        case REDIS_ERR_IO:
            atom = ATOM_REDIS_ERR_IO;
            break;
        case REDIS_ERR_EOF:
            atom = ATOM_REDIS_ERR_EOF;
            break;
        case REDIS_ERR_PROTOCOL:
            atom = ATOM_REDIS_ERR_PROTOCOL;
            break;
        case REDIS_ERR_OOM:
            atom = ATOM_REDIS_ERR_OOM;
            break;
        case REDIS_ERR_OTHER:
            atom = ATOM_REDIS_ERR_OTHER;
            break;
    }

    return enif_make_tuple2(env, ATOM_ERROR, enif_make_tuple2(env, atom, enif_make_string(env, reason, ERL_NIF_LATIN1)));
};

static ERL_NIF_TERM hierdis_make_binary_from_reply(ErlNifEnv* env, redisReply* r)
{
    ERL_NIF_TERM term;
    
    hiredis_reply_handle* handle = (hiredis_reply_handle*)enif_alloc_resource(HIREDIS_REPLY_RESOURCE, sizeof(hiredis_reply_handle)); 
    handle->reply = r;
    term = enif_make_resource_binary(env, handle, handle->reply->str, handle->reply->len);    
    enif_release_resource(handle);

    return term;
}

static ERL_NIF_TERM hierdis_make_list_from_reply(ErlNifEnv* env, redisReply* r) 
{
    ERL_NIF_TERM list[r->elements];

    for(int i = 0; i < r->elements; i++)
    {
        list[i] = hierdis_make_response(env, r->element[i], false);
    }

    return enif_make_list_from_array(env, list, r->elements);
};

static ERL_NIF_TERM hierdis_make_response(ErlNifEnv* env, redisReply* r, bool as_copy)
{
    ERL_NIF_TERM term;

    switch(r->type)
    {
        case REDIS_REPLY_STRING:
        case REDIS_REPLY_STATUS:
            term = hierdis_make_binary_from_reply(env, r);
            break;
        case REDIS_REPLY_ARRAY:
            term = hierdis_make_list_from_reply(env, r);
            hierdis_free_reply(r);
            break;
        case REDIS_REPLY_INTEGER:
            term = enif_make_int64(env, r->integer);
            hierdis_free_reply(r);
            break;
        case REDIS_REPLY_NIL:
            term = enif_make_atom(env, "undefined\0");
            hierdis_free_reply(r);
            break;
        default:
            term = hierdis_make_error(env, REDIS_REPLY_ERROR, "Unknown reply error.");
            hierdis_free_reply(r);
    }
    return term;
};

static ERL_NIF_TERM connect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    unsigned int length;

    if(!enif_get_list_length(env, argv[0], &length)) 
    {
        return enif_make_badarg(env);
    }

    char ip[length+1];
    int port;
    int timeout;

    if(enif_get_string(env, argv[0], ip, length+1, ERL_NIF_LATIN1) && enif_get_int(env, argv[1], &port))
    {
        hiredis_context_handle* handle = (hiredis_context_handle*)enif_alloc_resource(HIREDIS_CONTEXT_RESOURCE, sizeof(hiredis_context_handle)); 
        
        if(argc == 3 && enif_get_int(env, argv[2], &timeout))
        {
            struct timeval sec = {timeout, 0}; // timeout in num seconds
            handle->context = redisConnectWithTimeout(ip, port, sec);
        }
        else
        {
            handle->context = redisConnect(ip, port);            
        }
        
        if (handle->context != NULL && handle->context->err) 
        {
            ERL_NIF_TERM error = hierdis_make_error(env, handle->context->err, handle->context->errstr);
            enif_release_resource(handle);
            return error;
        }
        else
        {
            ERL_NIF_TERM result = enif_make_resource(env, handle);
            enif_release_resource(handle);
            return enif_make_tuple2(env, ATOM_OK, result);
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
};

static ERL_NIF_TERM connect_unix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    unsigned int length;

    if(!enif_get_list_length(env, argv[0], &length)) 
    {
        return enif_make_badarg(env);
    }

    char socket_path[length+1];
    int timeout;

    if(enif_get_string(env, argv[0], socket_path, length+1, ERL_NIF_LATIN1))
    {
        hiredis_context_handle* handle = (hiredis_context_handle*)enif_alloc_resource(HIREDIS_CONTEXT_RESOURCE, sizeof(hiredis_context_handle)); 
        if(argc == 3 && enif_get_int(env, argv[2], &timeout))
        {
            struct timeval sec = {timeout, 0}; // timeout in num seconds
            handle->context = redisConnectUnixWithTimeout(socket_path, sec);
        }
        else
        {
            handle->context = redisConnectUnix(socket_path);            
        }
        
        if (handle->context != NULL && handle->context->err) 
        {
            ERL_NIF_TERM error = hierdis_make_error(env, handle->context->err, handle->context->errstr);
            enif_release_resource(handle);
            return error;
        }
        else
        {
            ERL_NIF_TERM result = enif_make_resource(env, handle);
            enif_release_resource(handle);
            return enif_make_tuple2(env, ATOM_OK, result);
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
};

static int list_to_hiredis_argv(ErlNifEnv* env, ERL_NIF_TERM list, unsigned int argc, const char* argv[], size_t argv_lengths[])
{
    ERL_NIF_TERM head, tail;
    ErlNifBinary list_elm;

    for(int i = 0; i < argc; i++)
    {
        enif_get_list_cell(env, list, &head, &tail);
        if (!enif_inspect_iolist_as_binary(env, head, &list_elm))
        {
          return false;
        }
        
        argv[i] = (const char*)list_elm.data;
        argv_lengths[i] = list_elm.size;

        list = tail;
    }
    return true;
};

static ERL_NIF_TERM command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    hiredis_context_handle* handle;
    redisReply* reply;
    unsigned int hiredis_argc;
    
    if (!enif_get_list_length(env, argv[1], &hiredis_argc)) 
    {
        return enif_make_badarg(env);    
    }

    const char* hiredis_argv[hiredis_argc];
    size_t hiredis_argv_lengths[hiredis_argc];

    if(enif_get_resource(env, argv[0], HIREDIS_CONTEXT_RESOURCE, (void**)&handle))
    {
        if (!list_to_hiredis_argv(env, argv[1], hiredis_argc, hiredis_argv, hiredis_argv_lengths))
        {
            return enif_make_badarg(env);
        }
        reply = redisCommandArgv(handle->context, hiredis_argc, hiredis_argv, hiredis_argv_lengths);
        
        if (handle->context != NULL && handle->context->err) 
        {
            return hierdis_make_error(env, handle->context->err, handle->context->errstr);
        }
        else if(reply->type == REDIS_REPLY_ERROR)
        {
            return hierdis_make_error(env, REDIS_REPLY_ERROR, reply->str);
        }
        else
        {
            return enif_make_tuple2(env, ATOM_OK, hierdis_make_response(env, reply, false));
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
};

static ERL_NIF_TERM append_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    hiredis_context_handle* handle;
    unsigned int hiredis_argc;
    
    if (!enif_get_list_length(env, argv[1], &hiredis_argc)) 
    {
        return enif_make_badarg(env);    
    }

    const char* hiredis_argv[hiredis_argc];
    size_t hiredis_argv_lengths[hiredis_argc];

    if(enif_get_resource(env, argv[0], HIREDIS_CONTEXT_RESOURCE, (void**)&handle))
    {
        if (!list_to_hiredis_argv(env, argv[1], hiredis_argc, hiredis_argv, hiredis_argv_lengths))
        {
            return enif_make_badarg(env);
        }
        redisAppendCommandArgv(handle->context, hiredis_argc, hiredis_argv, hiredis_argv_lengths);
        if (handle->context != NULL && handle->context->err) 
        {
            return hierdis_make_error(env, handle->context->err, handle->context->errstr);
        }
        else
        {
            return enif_make_tuple2(env, ATOM_OK, enif_make_int(env, sdslen(handle->context->obuf)));
        }
    }
    else
    {
        return enif_make_badarg(env);
    }
};

static ERL_NIF_TERM get_reply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    hiredis_context_handle* handle;

    if(enif_get_resource(env, argv[0], HIREDIS_CONTEXT_RESOURCE, (void**)&handle))
    {
        redisReply* reply;

        redisGetReply(handle->context, (void*)&reply); 
        if (handle->context != NULL && handle->context->err) 
        {
            return hierdis_make_error(env, handle->context->err, handle->context->errstr);
        }
        else if(reply->type == REDIS_REPLY_ERROR)
        {
            return hierdis_make_error(env, REDIS_REPLY_ERROR, reply->str);
        }
        else
        {
            return enif_make_tuple2(env, ATOM_OK, hierdis_make_response(env, (redisReply*)reply, false));
        }   
    }
    else
    {
        return enif_make_badarg(env);
    }
};

static ErlNifFunc nif_funcs[] = 
{
    {"connect", 2, connect},
    {"connect", 3, connect},
    {"connect_unix", 1, connect_unix},
    {"connect_unix", 2, connect_unix},

    {"command", 2, command},
    {"append_command", 2, append_command},

    {"get_reply", 1, get_reply}
};

void hierdis_free_reply(void *reply)
{
    redisReply *r = reply;

    switch(r->type) {
    case REDIS_REPLY_INTEGER:
        break; /* Nothing to free */
    case REDIS_REPLY_ARRAY: // Removed the recursive free behavior, because we're allowing Erlang to do GC instead.
    case REDIS_REPLY_ERROR:
    case REDIS_REPLY_STATUS:
    case REDIS_REPLY_STRING:
        if (r->str != NULL)
            free(r->str);
        break;
    }
    free(r);
}

void hiredis_context_handle_dtor(ErlNifEnv* env, void* arg)
{
    hiredis_context_handle* handle = (hiredis_context_handle*)arg;
    redisFree(handle->context);
}

void hiredis_reply_handle_dtor(ErlNifEnv* env, void* arg)
{
    hiredis_reply_handle* handle = (hiredis_reply_handle*)arg;
    hierdis_free_reply(handle->reply);
}

static int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) 
{
    // Initialize common atoms
    #define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
        ATOM(ATOM_OK, "ok");
        ATOM(ATOM_ERROR, "error");
        ATOM(ATOM_BADARG, "badarg");
        ATOM(ATOM_TRUE, "true");
        ATOM(ATOM_FALSE, "false");
        ATOM(ATOM_REDIS_ERR_IO, "redis_err_io");
        ATOM(ATOM_REDIS_ERR_EOF, "redis_err_eof");
        ATOM(ATOM_REDIS_ERR_PROTOCOL, "redis_err_protocol");
        ATOM(ATOM_REDIS_ERR_OOM, "redis_err_oom");
        ATOM(ATOM_REDIS_ERR_OTHER, "redis_err_other");
        ATOM(ATOM_REDIS_REPLY_ERROR, "redis_reply_error");
    #undef ATOM

    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    HIREDIS_CONTEXT_RESOURCE = enif_open_resource_type(
                                env,
                                NULL, 
                                "hierdis_context_resource", 
                                &hiredis_context_handle_dtor, 
                                flags,
                                NULL
                            );

    HIREDIS_REPLY_RESOURCE = enif_open_resource_type(
                                env,
                                NULL, 
                                "hierdis_reply_resource", 
                                &hiredis_reply_handle_dtor, 
                                flags,
                                NULL
                            );

    return 0;
}

ERL_NIF_INIT(hierdis, nif_funcs, on_nif_load, NULL, NULL, NULL);
