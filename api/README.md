# API

## Cheatsheet for Devs

```bash
# Generate Base Migration File
sea-orm-cli migrate generate MIGRATION_NAME
# Setup Database
docker run --rm --name blee-db -e POSTGRES_PASSWORD=BleePasswd -e POSTGRES_DB=blee -e POSTGRES_USER=blee --expose 5432  -p -d postgres
# Apply Migration
DATABASE_URL="postgresql://blee:BleePasswd@localhost:5432/blee" sea-orm-cli migrate refresh
# Generate Models from Database
sea-orm-cli generate entity --database-url "postgresql://blee:BleePasswd@localhost:5432/blee"  --output-dir entity/src/entities --with-serde both
```
