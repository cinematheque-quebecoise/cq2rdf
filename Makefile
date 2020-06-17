create-release:
	curl --request POST --form token=062835f38071efce2722fbec61d974 --form ref=master https://gitlab.com/api/v4/projects/18031890/trigger/pipeline
