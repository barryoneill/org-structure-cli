# org-structure-cli

Scripts for managing org structure data

To use, execute the script in a directory containing your org structure.

Your org structure must be contained in the following three csv files:
* members.csv
* titles.csv
* teams.csv

The first row of the file contains the field names. The following rules
apply to the field names:
* A field name followed by '(m)' is considered multi-value. Values in this
field are pipe delimitted.
* A field name followed by '(id)' is considered the id field. Any field can
be the id field but there can only be one per file and values in it must be
unique. It cannot be multi-value.
* A field called 'Title' indicates a reference to the id field in titles.csv. It
cannot be multi-value.
* A field called 'Team' indicates a reference to the id field in teams.csv. It
cannot be multi-value.
* A field called 'Member' indicates a reference to the id field in members.csv.
It cannot be multi-value.


## Usage

To validate the structure of your files (i.e.that references are valid, ids are unique, etc):
`org validate`

To add a new member:
`org add member [id]`

To update a member single-value field:
`org update member [id] [field] [value]`

To update a member multi-value field:
`org update member [id] add|remove [field] [value]`

To remove a member:
`org remove member [id]`

To fetch a member by id:
`org get member [id]`

To find a member using a substring match on a field value:
`org find member [field] [value]`

Each action will create a new commit with a consistent, easy to parse name. This allows
changes to be published or other actions to be taken via GitHub webhooks.
