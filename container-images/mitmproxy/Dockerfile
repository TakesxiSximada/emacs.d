# build: docker build -t mitmproxy:202509 .
FROM python:3.13-slim-bookworm

WORKDIR /
COPY ./requirements.txt ./constraints.txt ./
RUN --mount=type=cache,target=/root/.cache/pip \
    pip install -r ./requirements.txt -c constraints.txt

COPY ./main.py .
CMD ["bash"]
