FROM rocker/r-ver:4.5.2

ENV DEBIAN_FRONTEND=noninteractive \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 \
    build-essential \
    cmake \
    gfortran \
    libblas-dev \
    liblapack-dev \
    libomp-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 \
    shiny DT ggplot2 dplyr stringr glmnet rpart partykit ranger xgboost nnet jsonlite

WORKDIR /app
COPY SU2026_prediction_app/ SU2026_prediction_app/
COPY SU2026_prediction_web/ SU2026_prediction_web/

ENV SU2026_PORTABLE_ARTIFACT=1 \
    SU2026_BRIDGE_ONLY=1 \
    HOST=0.0.0.0 \
    PORT=10000

EXPOSE 10000
CMD ["python3", "SU2026_prediction_web/server.py"]
