# ShinyApps
This is a package of three applications: TeloBase, IntensityAnalyser, and ScanToIntensity.

## Prerequisites
You need to have docker (compose) installed.

## Configuration
The TeloBase application uses secrets for the SMTP server. To configure it, create a `.env` file and fill in the appropriate values:

```
SMTP_HOST=XXX
SMTP_PORT=XXX
SMTP_USER=XXX
SMTP_PASS=XXX
```

## Building the docker image

Simply run `docker build -t shinyapps .` to build the image.

## Running the applications

Once the image is ready, use `docker compose up` to fire up the containers.

The applications are then available at:

- `localhost:8080/TeloBase/`
- `localhost:8080/IntensityAnalyser/`
- `localhost:8080/ScanToIntensity/`
