
# Minio S3

## Table of Contents

- [Minio S3](#minio-s3)
  - [Table of Contents](#table-of-contents)
  - [Minio S3 SetUp](#minio-s3-setup)

## Minio S3 SetUp

para iniciar minio server y cliente
agrega automaticamente README a MYSTORAGE/test

```bash
docker compose up
```

Now, you can access the mc client anytime:

```bash
docker exec -it client /bin/sh
```

Inside the container, para ver archivos, run:

```bash
mc ls MYSTORAGE
mc ls MYSTORAGE/test
```

Se puede usar el API en

hpps://localhost:9000

y webUI

<http://127.0.0.1:9001>

con

USER=minioadmin
PASSWORD=minioadmin

Docs: <https://docs.min.io>
