from starlette.applications import Starlette
from starlette.endpoints import HTTPEndpoint
from starlette.responses import PlainTextResponse
from starlette.routing import Route


def setup_func():
    pass


class PingPoingEndpoint(HTTPEndpoint):
    async def get(self, request):
        def _background():
            pass

        return PlainTextResponse("pong", background=_background, status_code=200)


app = Starlette(
    debug=True,
    routes=[
        Route("/", PingPoingEndpoint),
    ],
    on_startup=[
        setup_func,
    ],
)
