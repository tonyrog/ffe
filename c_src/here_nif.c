//
// FFE word builder (1-indexed!)
//

#include <stdint.h>
#include <memory.h>

#include "erl_nif.h"

// #define DEBUG

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)

DECL_ATOM(ok);

static int here_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int here_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int here_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info);
static void here_unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM here_new(ErlNifEnv* env, int argc, 
			     const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_allot(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_here(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_size(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_clear(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_store(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_fetch(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_comma(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_copy(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM here_trim(ErlNifEnv* env, int argc, 
			      const ERL_NIF_TERM argv[]);


#define DEFAULT_SIZE 4
#define EXTRA_SIZE   4

ErlNifFunc here_funcs[] =
{
    { "new",     0, here_new   },
    { "new",     1, here_new   },
    { "allot",   2, here_allot },
    { "size",    1, here_size  },
    { "here",    1, here_here  },
    { "clear",   1, here_clear },
    { "store",   3, here_store },
    { "fetch",   2, here_fetch },
    { "comma",   2, here_comma },
    { "copy",    1, here_copy  },
    { "trim",    1, here_trim  },
};

static ErlNifResourceType* here_res;
static ERL_NIF_TERM zero;


typedef struct {
    ErlNifEnv*    env;
    unsigned long  size;  // number of cells allocated
    unsigned long  dp;    // next cell to use
    ERL_NIF_TERM* cell;
} here_object_t;

static void h_dtor(ErlNifEnv* env, here_object_t* obj)
{
    DBG("h_dtor\r\n");
    enif_free(obj->cell);
    enif_free_env(obj->env);
}

static long h_realloc(here_object_t* obj, long new_size)
{
    ERL_NIF_TERM* cell;
    int i;
    if (!(cell = enif_realloc(obj->cell, sizeof(ERL_NIF_TERM)*new_size)))
	return -1;
    for (i = obj->size; i < new_size; i++)
	cell[i] = zero;
    obj->size = new_size;
    if (obj->dp > obj->size)
	obj->dp = obj->size;
    obj->cell = cell;
    return new_size;
}

static long h_allot(here_object_t* obj, long n)
{
    long dp1 = obj->dp + n;
    if (dp1 <= 0)
	dp1 = 0;
    else if (dp1 > obj->size) {
	if (h_realloc(obj, dp1+EXTRA_SIZE) < 0)
	    return -1;
    }
    obj->dp = dp1;
    return dp1;
}

static void h_clear(here_object_t* obj, int trim)
{
    obj->dp = 0;
    enif_clear_env(obj->env);
    if (trim) {
	enif_free(obj->cell);
	obj->cell = NULL;
	obj->size = 0;
    }
}


ERL_NIF_TERM here_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t initial_size = DEFAULT_SIZE;
    here_object_t* obj = NULL;
    ERL_NIF_TERM r;

    if (argc >= 1) {
	unsigned long val;
	if (!enif_get_ulong(env, argv[0], &val))
	    goto error;
	initial_size = val;
    }
    if (!(obj = enif_alloc_resource(here_res, sizeof(here_object_t))))
	goto error;
    memset(obj, 0, sizeof(here_object_t));
    
    if (!(obj->env = enif_alloc_env()))
	goto error;
    if (h_realloc(obj, initial_size) < 0)
	goto error;
    r = enif_make_resource(env, obj);
    enif_release_resource(obj);
    return r;
    
error:
    if (obj)
	enif_release_resource(obj);
    return enif_make_badarg(env);
}

// move here position and possibly realloc
ERL_NIF_TERM here_allot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    long n;

    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_long(env, argv[1], &n))
	return enif_make_badarg(env);
    if ((n = h_allot(obj, n)) < 0)
	return enif_make_badarg(env);
    return enif_make_long(env, n);
}

ERL_NIF_TERM here_here(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    return enif_make_ulong(env, obj->dp+1);
}

ERL_NIF_TERM here_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    return enif_make_ulong(env, obj->size);
}

ERL_NIF_TERM here_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    h_clear(obj, 0);
    return ATOM(ok);
}

ERL_NIF_TERM here_store(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    int index;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index) ||
	(index < 1) || (index > (int)obj->size))
	return enif_make_badarg(env);
    obj->cell[index-1] = enif_make_copy(obj->env, argv[2]);
    return ATOM(ok);
}

ERL_NIF_TERM here_fetch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    int index;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index) ||
	(index < 1) || (index > (int)obj->size))
	return enif_make_badarg(env);
    return enif_make_copy(env, obj->cell[index-1]);
}

ERL_NIF_TERM here_comma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    unsigned long i;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (obj->dp >= obj->size) {
	if (h_realloc(obj, obj->dp+EXTRA_SIZE) < 0)
	    return enif_make_badarg(env);
    }
    i = obj->dp++;
    obj->cell[i] = enif_make_copy(obj->env, argv[1]);
    return enif_make_ulong(env, i+1);
}

ERL_NIF_TERM here_copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
	
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    {
        ERL_NIF_TERM cell[obj->dp];
	int i;
	for (i = 0; i < obj->dp; i++)
	    cell[i] = enif_make_copy(env, obj->cell[i]);
	return enif_make_tuple_from_array(env, cell, obj->dp);
    }
}

ERL_NIF_TERM here_trim(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;

    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (obj->dp == 0)
	h_clear(obj, 1);
    else {
	if (h_realloc(obj, obj->dp) < 0)
	    return enif_make_badarg(env);
    }
    return ATOM(ok);
}

static int here_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    ErlNifResourceFlags tried;
    
    DBG("here_load\r\n");

    zero = enif_make_int(env, 0);
    LOAD_ATOM(ok);    

    here_res = enif_open_resource_type(env, 0, "here",
				       (ErlNifResourceDtor*) h_dtor,
				       ERL_NIF_RT_CREATE,
				       &tried);
    *priv_data = 0;
    return 0;
}

static int here_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("here_reload\r\n");
    return 0;
}

static int here_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("here_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void here_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    DBG("here_unload\r\n");
}

ERL_NIF_INIT(here, here_funcs,
	     here_load, here_reload, 
	     here_upgrade, here_unload)
