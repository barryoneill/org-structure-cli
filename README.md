# org-structure-cli

Scripts for managing org structure data

Your org structure must be contained in the following three csv files:
* members.csv
* titles.csv
* teams.csv

Set ORG_DATA_DIR environment variable to the location of these files. (Defaults to the current directory.)

The first row of the file contains the field names. The following rules
apply to the field names:
* A field name followed by '(multi)' is considered multi-value. Values in this
field are pipe delimitted.
* A field name followed by '(id)' is considered the id field. There must be one
id field in each file. Any field can be the id field but there can only be one
per file and values in it must be unique. It cannot be multi-value.
* A field name followed by '(member)' indicates a reference to the id field in
members.csv. It cannot be multi-value.
* A field name followed by '(team)' indicates a reference to the id field in
teams.csv. It cannot be multi-value.
* A field name followed by '(title)' indicates a reference to the id field in
titles.csv. It cannot be multi-value.


## Usage

To run a general search across all fields:

`org members|teams|titles [value]`

To list all ids:

`org memberids|teamids|titleids`

To list all data:

`org members|teams|titles`

To fetch a member|team|title by id:

`org get member|team|title [id]`

To find a member using a substring match on a field value:

`org find members [field] [value]`

To validate the structure and integrity of your files (i.e.that references are valid, ids are unique, etc):

`org validate`

## Docker

A Dockerfile is provided so you can create an image with the cli.
