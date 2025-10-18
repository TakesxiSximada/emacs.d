from mitmproxy import http


def request(flow: http.HTTPFlow) -> None:
    if flow.request.path == "/aws":
        flow.response = http.Response.make(200, content=b"{}")
        return
    flow.response = http.Response.make(200, content=b"Hello, world!")
