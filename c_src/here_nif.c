//
// FFE word builder
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


#define DEFAULT_SIZE 64

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
};

static ErlNifResourceType* here_res;
static ERL_NIF_TERM zero;


typedef struct {
    ErlNifEnv*    env;
    unsigned long  size;  // number of cells allocated
    unsigned long  here;  // number of cells used (next cell to use)
    ERL_NIF_TERM* cell;
} here_object_t;

static void here_dtor(ErlNifEnv* env, here_object_t* obj)
{
    DBG("here_dtor\r\n");
    enif_free(obj->cell);
    enif_free_env(obj->env);
}

ERL_NIF_TERM here_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t initial_size = DEFAULT_SIZE;
    here_object_t* obj = NULL;
    ERL_NIF_TERM r;
    int i;

    if (argc >= 1) {
	unsigned long val;
	if (!enif_get_ulong(env, argv[0], &val))
	    goto error;
	if (val > 0)
	    initial_size = val;
    }
    if (!(obj = enif_alloc_resource(here_res, sizeof(here_object_t))))
	goto error;
    memset(obj, 0, sizeof(here_object_t));
    
    if (!(obj->env = enif_alloc_env()))
	goto error;
    if (!(obj->cell = enif_alloc(sizeof(ERL_NIF_TERM)*initial_size)))
	goto error;
    obj->size = initial_size;
    for (i = 0; i < initial_size; i++)
	obj->cell[i] = zero;    
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
    unsigned long new_here = 0;
    here_object_t* obj;
    long n;

    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_long(env, argv[1], &n))
	return enif_make_badarg(env);

    if (n < 0) {
	n = -n;
	if (obj->here <= n)
	    new_here = 0;
	else
	    new_here -= n;
    }
    else if (n > 0) {
	new_here = obj->here + n;
	if (new_here > obj->size) {
	    unsigned long new_size = new_here + 128;
	    ERL_NIF_TERM* new_cell;
	    int i;
	    if (!(new_cell = enif_realloc(obj->cell, new_size)))
		return enif_make_badarg(env);
	    for (i = obj->size; i < new_size; i++)
		new_cell[i] = zero;
	    obj->size = new_size;
	    obj->cell = new_cell;
	}
    }
    obj->here = new_here;
    return enif_make_ulong(env, new_here);
}

ERL_NIF_TERM here_here(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    return enif_make_ulong(env, obj->here);
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
    obj->here = 0;
    enif_clear_env(obj->env);
    return ATOM(ok);
}

ERL_NIF_TERM here_store(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    int index;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index) ||
	(index < 0) || (index >= (int)obj->size))
	return enif_make_badarg(env);
    obj->cell[index] = enif_make_copy(obj->env, argv[2]);
    return ATOM(ok);
}

ERL_NIF_TERM here_fetch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    int index;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (!enif_get_int(env, argv[1], &index) ||
	(index < 0) || (index >= (int)obj->size))
	return enif_make_badarg(env);
    return enif_make_copy(env, obj->cell[index]);
}

ERL_NIF_TERM here_comma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
    unsigned long index;
    
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    if (obj->here >= obj->size)
	return enif_make_badarg(env);
    index = obj->here++;
    obj->cell[index] = enif_make_copy(obj->env, argv[1]);
    return enif_make_ulong(env, index);
}

ERL_NIF_TERM here_copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    here_object_t* obj;
	
    if (!enif_get_resource(env, argv[0], here_res, (void**)&obj))
	return enif_make_badarg(env);
    {
        ERL_NIF_TERM cell[obj->here];
	int i;
	for (i = 0; i < obj->here; i++)
	    cell[i] = enif_make_copy(env, obj->cell[i]);
	return enif_make_tuple_from_array(env, cell, obj->here);
    }
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
				       (ErlNifResourceDtor*) here_dtor,
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
