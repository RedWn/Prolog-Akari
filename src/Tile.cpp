#include <Tile.h>
#include <SWI-cpp.h>
#include<Grid.h>
using namespace gl;
Tile::Tile(int x, int y) : x(x), y(y)
{
    update();
}
void Tile::draw(){
    glUniform1i(GRID.uniformsLocations[Uniforms::X], x);
    glUniform1i(GRID.uniformsLocations[Uniforms::Y], y);
    glBindTexture(GL_TEXTURE_2D, GRID.textures[type]);
    glDrawArrays(GL_POINTS, 0, 1);
    assert(glGetError() == 0);
}
void Tile::update(){
    char buffer[128];
    PlTerm value;
    if (PlCall("wall_num", PlTermv(PlTerm(long(x)), PlTerm(long(y)), value)))
    {
        switch ((int)value)
        {
        case 0:
            type = WALL_0;
            break;
        case 1:
            type = WALL_1;
            break;
        case 2:
            type = WALL_2;
            break;
        case 3:
            type = WALL_3;
            break;
        case 4:
            type = WALL_4;
            break;
        }
        wallVal = value;
        return;
    }
    if (PlCall("wall", PlTermv(PlTerm(long(x)), PlTerm(long(y)))))
    {
        type = WALL;
        return;
    }

    if (sprintf(buffer, "light(cell(%d,%d))", x, y) &&
        PlCall(buffer))
    {
        type = LIGHT;
        return;
    }
    if (sprintf(buffer, "is_cell_lit(cell(%d, %d))", x, y) &&
        PlCall(buffer))
    {
        type = LIT_VOID;
        return;
    }
    type = VOID;
}