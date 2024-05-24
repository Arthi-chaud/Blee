# API

## Cheatsheet for Devs

```bash
# Generate Base Migration File
sea-orm-cli migrate generate MIGRATION_NAME
# Setup Database
docker run --rm --name blee-db -e POSTGRES_PASSWORD=BleePasswd -e POSTGRES_DB=blee -e POSTGRES_USER=blee -p 5432:5432 -d postgres
# Apply Migration
DATABASE_URL="postgresql://blee:BleePasswd@localhost:5432/blee" sea-orm-cli migrate refresh
# Generate Models from Database
sea-orm-cli generate entity --database-url "postgresql://blee:BleePasswd@localhost:5432/blee"  --output-dir entity/src/entities --with-serde both
```

```bash
# Setup Containers
docker run --rm --name blee-mq -e RABBITMQ_DEFAULT_USER=blee  -e RABBITMQ_DEFAULT_PASS=blee -p 5672:5672 -d rabbitmq:3.13-alpine; docker run --rm --name blee-db -e POSTGRES_PASSWORD=BleePasswd -e POSTGRES_DB=blee -e POSTGRES_USER=blee -p 5432:5432  -d postgres:alpine3.16
# Run Tests
DATABASE_URL="postgresql://blee:BleePasswd@localhost:5432/blee" ROCKET_DATABASES="{sea_orm={url=$DATABASE_URL, pool_size=1}}" RABBIT_HOST=localhost RABBIT_PORT=5672 RABBIT_USER=blee RABBIT_PASS=blee CONFIG_DIR=./data SCANNER_API_KEY=a MATCHER_API_KEY=b cargo test --tests $SuiteName -- --test-threads=1 --show-output
```
