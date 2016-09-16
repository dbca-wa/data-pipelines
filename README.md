# data-pipelines
Reproducible insight and scientific programming

## First-time setup of Rstudio Server for Parks and Wildlife staff
Before you can access RStudio Server, follow the next section.
If you wish to setup this repository from a local RStudio Desktop installation,
skip the next section.

### Setup kens-xenmate-dev
On Parks and Wildlife's [remote desktop](https://guacamole.dpaw.wa.gov.au/),
select "Linux Mint SSH", enter your Parks and Wildlife username (not email) and
password, then type:
```
git config --global user.name "your name"
git config --global user.email "your email"
git config --global push matching
mkdir projects
logout
```
Close the browser tab. This operation achieved the following:

* By logging in successfully, a home directory was created on kens-denmate-dev.
  RStudio Server will place your R projects into this home directory, 
  or in any subfolder inside it.
* You have told the version control software Git your name and email, which will
  save you some typing in the future.
* You have configured Git to match branch names, which will save you some more typing.
* The new directory "projects" inside your home folder will hold your R projects
  and keep your home directory neat and uncluttered.
  
### Setup SSH
* Create a Github account
* [RStudio Server](https://rstudio.dpaw.wa.gov.au/) > Tools > Global Options 
  > Git > Create RSA key > no password > view public key > copy
* Github > Your icon > Settings >  SSH keys > New SSH key: paste public key into
  "Key", copy last line (username@kens-xenmate-dev) as "Title", save.
* Notify Florian Mayer to add your Github account as collaborator to this repository
  to gain write access.

## Setup of data-pipelines
In either your local RStudio Desktop, or in Parks and Wildlife's 
[RStudio Server](https://rstudio.dpaw.wa.gov.au/):

* Create new project from version control (Git)
* Repository URL: the SSH URL from this repo `git@github.com:parksandwildlife/data-pipelines.git`
* Project directory name: should automatically be `data-pipelines`
* Create project as subdirectory of: Browse > `projects`

## Use and contribute
We will work on one branch ("master"), as every collaborator's files are separated
by folders. This will make merges trivially simple.

Before you commence work, pull and merge the latest changes:

* Facet Git > Pull
* Commit merge if promted
* Push

Commence your work, which will change files. When done:
* Add, commit
* Pull, merge if necessary, commit
* Push
