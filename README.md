# Cloud Build State

A simple key-value storage for storing data between GCP Cloud Build steps. The executable is statically linked so that it will work on different Linux distros. Tested with Debian, Ubuntu ja Alpine.

## Usage

### Before use

Before using you have to build the image for your GCP project. It takes about 15 minutes.

```bash
gcloud builds submit --timeout=1800 --tag gcr.io/<PROJECT_ID>/cloudbuild-state
```

After that you can start using it by adding following as the first step in your cloudbuild.yaml file. This installs the executable into `/workspace` directory.

```yaml
- id: 'install'
  name: 'gcr.io/<PROJECT_ID>/cloudbuild-state'
```

### Commands

```
/workspace/cloudbuild-state set [key] [value]
/workspace/cloudbuild-state get [key]
/workspace/cloudbuild-state list (--json)
```
