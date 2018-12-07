-record(data_scene, {
        id        = 0,
        type      = undef,      % box|sphere|mesh
        position  = {0,0,0},    % 位置
        rotation  = {0,0,0,1},  % 旋转
        % 以下玩为box|sphere的内容
        scale     = {0,0,0},    % 形状，当为sphere的时候取第一个数为半径
        % 以下为mesh的内容
        vtx       = [],         % 顶点坐标
        idx       = []          % 三角形坐标索引
    }).