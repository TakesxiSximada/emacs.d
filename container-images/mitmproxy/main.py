from mitmproxy import http


def request(flow: http.HTTPFlow) -> None:
    flow.response = http.Response.make(200, content=b"Hello, world!")
