#include <SFML/Window.hpp>
#include <glbinding/glbinding.h>
#include <SWI-cpp.h>
#include <glbinding/gl33/gl.h>
#include <Grid.h>
using namespace sf;
using namespace gl;
int main(int argc, char **argv)
{
    ContextSettings ctxSettings{};
    ctxSettings.antialiasingLevel = 0;
    ctxSettings.depthBits = 0;
    ctxSettings.majorVersion = 3;
    ctxSettings.minorVersion = 3;
    ctxSettings.sRgbCapable = false;
    ctxSettings.stencilBits = 0;
    Window window(VideoMode(WINDOW_SIDE, WINDOW_SIDE), "Akari", 7U, ctxSettings);
    glbinding::initialize(nullptr);
    PlEngine plEngine(2, argv);
    GRID.init();
    while (1)
    {
        Event event;
        while (window.pollEvent(event))
        {
            switch (event.type)
            {
            case Event::Closed:
                return 0;
                break;
            case Event::KeyPressed:
                if (event.key.code == Keyboard::Space)
                {
                    GRID.solve();
                }
            default:
                break;
            }
        }
        GRID.draw();
        window.display();
    }
}