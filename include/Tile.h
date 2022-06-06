#pragma once
class Tile{
    int x, y;

public:
    enum Type
    {
        LIGHT,
        WALL,
        NUM_WALL,
        VOID,
        NUM_TYPES
    };
    Tile(int x, int y, Type type);
    Type type;
    void draw();
};