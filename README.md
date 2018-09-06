# ST2018_WG_8

Upload your local folder as follows:

1) cd to/your/directory/with/your/name/folder
(something like SSVT, where SSVT contains a folder "Henk", don't go into Henk!)

4) setup your github identity

git config --global user.name "John Doe"
git config --global user.email johndoe@example.com

3) Initialize git folder:

git init

4) Add your files(Folder):

git add .

git commit -m "Your initial commit message"

5) Setup the remote origin:

git remote add origin git@github.com:software-engineering-amsterdam/ST2018_WG_8.git

6) Push your files:

git push origin master

7) Every other time you make changes use:
git add .
git commit -m "Message"
git push
