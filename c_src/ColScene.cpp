#include <list>
#include <math.h>
#include "ColScene.h"
#include "btBulletDynamicsCommon.h"

using namespace std;

//
struct MyColCallBack : btCollisionWorld::ContactResultCallback
{
    btVector3 m_colPos;
    bool is_col = false;

    btScalar addSingleResult(
        btManifoldPoint & cp,
        const btCollisionObjectWrapper * colObj0Wrap,
        int partId0,
        int index0,
        const btCollisionObjectWrapper * colObj1Wrap,
        int partId1,
        int index1)
    {
        m_colPos = cp.getPositionWorldOnB();
        is_col = true;
        printf("col pos {%f, %f, %f}\n", m_colPos.getX(), m_colPos.getY(), m_colPos.getZ());
        return btScalar(0.f);
    };
};

// n -------------------------------
// n -------------------------------
ColScene::ColScene()
{
    m_colPos = new btVector3();
    m_colConfig = new btDefaultCollisionConfiguration();
    m_dispatcher = new btCollisionDispatcher(m_colConfig);
    m_broadInterface = new btDbvtBroadphase();

    m_solver = new btSequentialImpulseConstraintSolver;
    m_world = new btDiscreteDynamicsWorld(m_dispatcher, m_broadInterface, m_solver, m_colConfig);
}

//
ColScene::~ColScene()
{
    for (int i = 0; i < m_world->getNumCollisionObjects(); ++i)
    {
        btCollisionObject* obj = m_world->getCollisionObjectArray()[i];
        deleteColObj(obj);
    }
    delete m_world;
    delete m_solver;
    delete m_broadInterface;
    delete m_dispatcher;
    delete m_colConfig;
    delete m_colPos;
}

// n -------------------------------
// n -------------------------------
void ColScene::deleteColObj(btCollisionObject* obj)
{
    btRigidBody* body = btRigidBody::upcast(obj);
    if (body && body->getMotionState())
    {
        delete body->getMotionState();
        delete body->getCollisionShape();
    }
    m_world->removeCollisionObject(obj);
    delete obj;
}

void ColScene::setColPos(btVector3 p)
{
    printf("pos = {%f, %f, %f}\n", p.getX(), p.getY(), p.getZ());
    m_colPos->setX(p.getX());
    m_colPos->setY(p.getY());
    m_colPos->setZ(p.getZ());
}

btRigidBody* ColScene::createAddRigidBody(float mass, const btTransform& startTransform, btCollisionShape* shape)
{
    bool isDynamic = (mass != 0.f);
    btVector3 localInertia(0, 0, 0);
    if (isDynamic)
        shape->calculateLocalInertia(mass, localInertia);

    btDefaultMotionState* myMotionState = new btDefaultMotionState(startTransform);
    btRigidBody::btRigidBodyConstructionInfo cInfo(mass, myMotionState, shape, localInertia);
    btRigidBody* body = new btRigidBody(cInfo);

    m_world->addRigidBody(body);
    return body;
};

// -----------------------
// -----------------------
bool ColScene::rayHit(btVector3 from, btVector3 to)
{
    btCollisionWorld::ClosestRayResultCallback callback(from, to);
    m_world->rayTest(from, to, callback);
    if(callback.hasHit())
    {
        btVector3 p = callback.m_hitPointWorld;
        setColPos(p);
        return true;
    }
    return false;
}

bool ColScene::checkPos(btCollisionObject* obj)
{
    MyColCallBack cb = MyColCallBack();
    m_world->contactTest(obj, cb);
    btVector3 pos = cb.m_colPos;
    setColPos(pos);
    return cb.is_col;
}

bool ColScene::checkFirstCupsule()
{
    bool is_col = false;

    m_world->performDiscreteCollisionDetection();
    // m_world->stepSimulation(1.f/60.f);

    list<btCollisionObject*> m_collisionObjects;
    int numManifolds = m_world->getDispatcher()->getNumManifolds();

    for(int i=0; i<numManifolds; i++)
    {
        btPersistentManifold* contactManifold = m_world->getDispatcher()->getManifoldByIndexInternal(i);
        btCollisionObject* obA = (btCollisionObject*)(contactManifold->getBody0());
        btCollisionObject* obB = (btCollisionObject*)(contactManifold->getBody1());

        int numContacts = contactManifold->getNumContacts();
        for(int j=0; j<numContacts; j++)
        {
            btManifoldPoint& pt = contactManifold->getContactPoint(j);
            if(pt.getDistance()<0.f)
            {
                m_collisionObjects.push_back(obA);
                m_collisionObjects.push_back(obB);
                btVector3 posA = pt.getPositionWorldOnA();
                btVector3 posB = pt.getPositionWorldOnB();
            }
        }
    }

    if(m_collisionObjects.size()>0)
    {
        m_collisionObjects.sort();
        m_collisionObjects.unique();
        for(list<btCollisionObject*>::iterator itr = m_collisionObjects.begin(); itr != m_collisionObjects.end(); ++itr) {
            btCollisionObject* colObj = *itr;

            if(colObj->getCollisionShape()->getShapeType()==CAPSULE_SHAPE_PROXYTYPE) // 如果是胶囊体刚体
            {
                btTransform trans = colObj->getWorldTransform();
                setColPos(trans.getOrigin());
                is_col = true;
                break;
            }
        }
        m_collisionObjects.clear();
    }
    return is_col;
}

// -------------------------------
// -------------------------------

btRigidBody* ColScene::addBox(btVector3 boxHalf, btVector3 bpos, btVector4 rota, btScalar mass)
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin(bpos);
    trans.setRotation(btQuaternion(rota.getX(), rota.getY(), rota.getZ(), rota.getW()));
    btCollisionShape* shape = new btBoxShape(boxHalf);
    return createAddRigidBody(mass, trans, shape);
};

btRigidBody* ColScene::addSphere(btScalar radius, btVector3 bpos, btVector4 rota, btScalar mass)
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin(bpos);
    trans.setRotation(btQuaternion(rota.getX(), rota.getY(), rota.getZ(), rota.getW()));
    btCollisionShape* shape = new btSphereShape(radius);
    return createAddRigidBody(mass, trans, shape);
};

btRigidBody* ColScene::addCupsule(
                                btScalar radius,
                                btScalar height,
                                btVector3 bpos, btVector4 rota, btScalar mass)
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin(bpos);
    trans.setRotation(btQuaternion(rota.getX(), rota.getY(), rota.getZ(), rota.getW()));
    btCollisionShape* shape = new btCapsuleShape((btScalar)radius, height);
    return createAddRigidBody(mass, trans, shape);
};

btRigidBody* ColScene::addTriMesh(
                                int vtxCount,
                                int idxCount,
                                btScalar vtx[],
                                unsigned short idx[],
                                btVector3 bpos,
                                btVector4 rota,
                                btScalar mass)
{
    btTransform trans;
    trans.setIdentity();
    trans.setOrigin(bpos);
    trans.setOrigin(btVector3(0,-25,0));
    trans.setRotation(btQuaternion(rota.getX(), rota.getY(), rota.getZ(), rota.getW()));

    btTriangleIndexVertexArray* meshInterface = new btTriangleIndexVertexArray();
    btIndexedMesh part;

    part.m_vertexBase = (const unsigned char*)vtx;
    part.m_vertexStride = sizeof(btScalar) * 3;
    part.m_numVertices = vtxCount;
    part.m_triangleIndexBase = (const unsigned char*)idx;
    part.m_triangleIndexStride = sizeof(short) * 3;
    part.m_numTriangles = idxCount/3;
    part.m_indexType = PHY_SHORT;

    meshInterface->addIndexedMesh(part, PHY_SHORT);

    bool useQuantizedAabbCompression = true;
    btBvhTriangleMeshShape* shape = new btBvhTriangleMeshShape(meshInterface, useQuantizedAabbCompression);

    return createAddRigidBody(mass, trans, shape);
};

// for debug
// -------------------------
btCollisionObject* ColScene::createCapsule(btVector3 posA, btVector3 posB, double radius)
{
    btScalar lenX = posB.getX() - posA.getX();
    btScalar lenY = posB.getY() - posA.getY();
    btScalar lenZ = posB.getZ() - posA.getZ();
    btScalar height = sqrt(lenX*lenX + lenY*lenY + lenZ*lenZ);

    btScalar posX = (posA.getX()+posB.getX())/2.f;
    btScalar posY = (posA.getY()+posB.getY())/2.f;
    btScalar posZ = (posA.getZ()+posB.getZ())/2.f;

    printf("lenX  -> %f\n", lenX );
    printf("lenY  -> %f\n", lenY );
    printf("lenZ  -> %f\n", lenZ );
    printf("heigh -> %f\n", height);

    printf("posX  -> %f\n", posX );
    printf("posY  -> %f\n", posY );
    printf("posZ  -> %f\n", posZ );

    btVector3 bpos(posX, posY, posZ);
    btVector4 rota(lenX, lenY, lenZ, 1.f);
    btScalar mass(1.f);

    btTransform trans;
    trans.setIdentity();
    trans.setOrigin(bpos);
    trans.setRotation(btQuaternion(rota.getX(), rota.getY(), rota.getZ(), rota.getW()));
    btCollisionShape* shape = new btCapsuleShape((btScalar)radius, height);

    bool isDynamic = (mass != 0.f);
    btVector3 localInertia(0, 0, 0);
    if (isDynamic)
        shape->calculateLocalInertia(mass, localInertia);

    btDefaultMotionState* myMotionState = new btDefaultMotionState(trans);
    btRigidBody::btRigidBodyConstructionInfo cInfo(mass, myMotionState, shape, localInertia);
    btRigidBody* body = new btRigidBody(cInfo);
    return body;
}