#ifndef COL_SCENE
#define COL_SCENE

#include "btBulletDynamicsCommon.h"

class ColScene
{
    private:
        btDefaultCollisionConfiguration* m_colConfig;
        btCollisionDispatcher* m_dispatcher;
        btBroadphaseInterface* m_broadInterface;
        btSequentialImpulseConstraintSolver* m_solver;
        btDynamicsWorld* m_world;  // 场景信息，退出的时候需要delete

        // add collision object
        btRigidBody* createAddRigidBody(btScalar mass, const btTransform& startTransform, btCollisionShape* shape);
        void deleteColObj(btCollisionObject* obj);
        void setColPos(btVector3 p);

    public:
        btVector3* m_colPos;       // 用于记录每次碰撞位置

        ColScene();
        ~ColScene();

        // for add
        btRigidBody* addBox(btVector3 boxHalf, btVector3 bpos, btVector4 rota, btScalar mass);
        btRigidBody* addSphere(btScalar radius, btVector3 bpos, btVector4 rota, btScalar mass);
        btRigidBody* addCupsule(
                                btScalar radius,
                                btScalar height,
                                btVector3 bpos, btVector4 rota, btScalar mass);
        btRigidBody* addTriMesh(
                                int vtxCount,
                                int idxCount,
                                btScalar vtx[],
                                unsigned short idx[],
                                btVector3 bpos, btVector4 rota, btScalar mass);
        // for check
        bool rayHit(btVector3 from, btVector3 to);
        bool checkPos(btCollisionObject* obj);
        bool checkFirstCupsule();

        // for debug
        // void printAllObj();
        btCollisionObject* createCapsule(btVector3 posA, btVector3 posB, double radius);
};
#endif