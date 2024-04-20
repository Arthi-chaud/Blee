# Front App

## Dev Environment

Start the root `docker-compose.dev.yml` and run:

```bash
flutter run -d chrome --web-port 3000 --web-hostname 0.0.0.0 --dart-define=API_URL=http://localhost:8000 --web-browser-flag "--disable-web-security"
```
