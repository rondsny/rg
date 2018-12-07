#include <stdio.h>
#include "erl_nif.h"
#include "btBulletDynamicsCommon.h"
#include "ColScene.h"

ColScene* g_scene;

// ---------------------------
static int get_number_f(ErlNifEnv *env, ERL_NIF_TERM eterm, double *f)
{
    if (enif_get_double(env, eterm, f)) {
        return 1;
    }
    else {
        long n;
        if (enif_get_long(env, eterm, &n)){
            *f = (double)n;
            return 1;
        }
        else {
            return 0;
        }
    }
}

static int get_vector3(ErlNifEnv *env, ERL_NIF_TERM eterm, btVector3 *vector)
{
    double x,y,z;
    int arity;
    const ERL_NIF_TERM *array;
    if (enif_get_tuple(env, eterm, &arity, &array)
        && 3 == arity
        && get_number_f(env, array[0], &x)
        && get_number_f(env, array[1], &y)
        && get_number_f(env, array[2], &z)) {

        vector->setX((btScalar)x);
        vector->setY((btScalar)y);
        vector->setZ((btScalar)z);

        return 1;
    }
    else {
        return 0;
    }
}

static int get_vector4(ErlNifEnv *env, ERL_NIF_TERM eterm, btVector4 *vector)
{
    double x,y,z,w;
    int arity;
    const ERL_NIF_TERM *array;
    if (enif_get_tuple(env, eterm, &arity, &array)
        && 4 == arity
        && get_number_f(env, array[0], &x)
        && get_number_f(env, array[1], &y)
        && get_number_f(env, array[2], &z)
        && get_number_f(env, array[3], &w)) {

        vector->setX((btScalar)x);
        vector->setY((btScalar)y);
        vector->setZ((btScalar)z);
        vector->setW((btScalar)w);

        return 1;
    }
    else {
        return 0;
    }
}

// ---------------------------

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM open_scene(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (!g_scene)
    {
        g_scene = new ColScene();
        printf("init scene done\n");
    }
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM close_scene(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    delete g_scene;
    return enif_make_atom(env, "ok");
}

// ---------------------------------------------------------

static ERL_NIF_TERM add_box(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    btVector3 boxHalf;
    btVector3 bpos;
    btVector4 rota;

    if(
        get_vector3(env, argv[0], &boxHalf) &&
        get_vector3(env, argv[1], &bpos) &&
        get_vector4(env, argv[2], &rota)
    ){
        g_scene->addBox(boxHalf, bpos, rota, 0.);
        return enif_make_atom(env, "ok");
    }
    return enif_make_atom(env, "undef");
}

static ERL_NIF_TERM add_sphere(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double radius;
    btVector3 bpos;
    btVector4 rota;

    if(
        enif_get_double(env, argv[0], &radius) &&
        get_vector3(env, argv[1], &bpos) &&
        get_vector4(env, argv[2], &rota)
    ){
        g_scene->addSphere(radius, bpos, rota, 0.);
        return enif_make_atom(env, "ok");
    }
    return enif_make_atom(env, "undef");
}

static ERL_NIF_TERM add_cupsule(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double radius;
    double height;
    btVector3 bpos;
    btVector4 rota;

    if(
        enif_get_double(env, argv[0], &radius) &&
        enif_get_double(env, argv[1], &height) &&
        get_vector3(env, argv[2], &bpos) &&
        get_vector4(env, argv[3], &rota)
    ){
        g_scene->addCupsule(radius, height, bpos, rota, 0.);
        return enif_make_atom(env, "ok");
    }
    return enif_make_atom(env, "undef");
}

static ERL_NIF_TERM add_mesh(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int vtxCount;
    int idxCount;
    btVector3 bpos;
    btVector4 rota;

    double vt;
    int id;
    btScalar vtx[10240];
    unsigned short idx[20480];

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;

    tail = argv[2];
    int i = 0;
    while(enif_get_list_cell(env, tail, &head, &tail))
    {
        enif_get_double(env, head, &vt);
        vtx[i] = (float)vt;
        i++;
    }

    tail = argv[3];
    i = 0;
    while(enif_get_list_cell(env, tail, &head, &tail)){
        enif_get_int(env, head, &id);
        idx[i] = (unsigned short)id;
        i++;
    }

    if(
        enif_get_int(env, argv[0], &vtxCount) &&
        enif_get_int(env, argv[1], &idxCount) &&
        get_vector3(env, argv[4], &bpos) &&
        get_vector4(env, argv[5], &rota)
    ){
        g_scene->addTriMesh(vtxCount, idxCount,
                            vtx, idx,
                            bpos, rota, 0.f);
        return enif_make_atom(env, "ok");
    }
    return enif_make_atom(env, "undef");
}

// ---------------------------------------------------------

// @return {1, {x,y,z}}| {0, {A.x, A.y, A.z}}
static ERL_NIF_TERM col_check_capsule(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    btVector3 posA;
    btVector3 posB;
    if (!get_vector3(env, argv[0], &posA))
        return enif_make_badarg(env);

    else
    {
        if (!get_vector3(env, argv[1], &posB))
            return enif_make_badarg(env);

        else
        {
            double radius;
            enif_get_double(env, argv[3], &radius);
            btCollisionObject* obj = g_scene->createCapsule(posA, posB, radius);
            if(g_scene->checkPos(obj))
            {
                ERL_NIF_TERM termPos = enif_make_tuple3(env,
                    enif_make_double(env, g_scene->m_colPos->getX()),
                    enif_make_double(env, g_scene->m_colPos->getY()),
                    enif_make_double(env, g_scene->m_colPos->getZ()));

                ERL_NIF_TERM terms = enif_make_tuple2(env, enif_make_int(env, 1), termPos);
                return terms;
            }

            ERL_NIF_TERM termA = enif_make_tuple3(env,
                enif_make_double(env, posA.getX()),
                enif_make_double(env, posA.getY()),
                enif_make_double(env, posA.getZ()));

            ERL_NIF_TERM terms = enif_make_tuple2(env, enif_make_int(env, 0), termA);
            return terms;
        }
    }
}


// check raycast hit
static ERL_NIF_TERM ray_hit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    btVector3 from;
    btVector3 to;
    if (!get_vector3(env, argv[0], &from))
        return enif_make_badarg(env);

    else
    {
        if (!get_vector3(env, argv[1], &to))
            return enif_make_badarg(env);

        else
        {
            if(g_scene->rayHit(from, to))
            {
                ERL_NIF_TERM termPos = enif_make_tuple3(env,
                    enif_make_double(env, g_scene->m_colPos->getX()),
                    enif_make_double(env, g_scene->m_colPos->getY()),
                    enif_make_double(env, g_scene->m_colPos->getZ()));

                ERL_NIF_TERM terms = enif_make_tuple2(env, enif_make_int(env, 1), termPos);
                return terms;
            }

            ERL_NIF_TERM termA = enif_make_tuple3(env,
                enif_make_double(env, from.getX()),
                enif_make_double(env, from.getY()),
                enif_make_double(env, from.getZ()));

            ERL_NIF_TERM terms = enif_make_tuple2(env, enif_make_int(env, 0), termA);
            return terms;
        }
    }
}


static ErlNifFunc nif_funcs[] = {
        {"open_scene", 0, open_scene},
        {"close_scene", 0, close_scene},

        {"add_box", 3, add_box},
        {"add_sphere", 3, add_sphere},
        {"add_cupsule", 4, add_cupsule},
        {"add_mesh", 6, add_mesh},

        {"col_check_capsule", 3, col_check_capsule},
        {"ray_hit", 2, ray_hit}
};


ERL_NIF_INIT(t_bullet, nif_funcs, load, NULL, upgrade, NULL)