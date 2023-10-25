# BPR Election Model 2024 Git Guide

## Table of Contents

1. [Git Workflow](#git-workflow)
2. [Git FAQs](#git-faqs)

## Git Workflow

### 1. Clone the repository

Open your terminal and change directories to where you want to store the project on your computer:

```bash
cd /path/to/your/directory
```

Run the following command in your terminal to clone the repository:

```bash
git clone  https://github.com/BPR-Data-Team/ElectionModel2024.git
```

Cloning the repository will create a new folder on your computer called `ElectionModel2024`. This folder will contain all of the code, data, and visualizations for the project. This is a one-time step. You will not need to clone the repository again.

Finally, change directories to the `ElectionModel2024` folder:

```bash
cd ElectionModel2024
```

**Each time you start working on the project, you should change directories to the `ElectionModel2024` folder.**

```bash
cd /path/to/your/directory/ElectionModel2024
```

**To check which directory you are currently in, run the following command:**

```bash
pwd
```

The directory you are currently in will be displayed in your terminal.

e.g. `/path/to/your/directory/ElectionModel2024`

### 2. Pull the latest changes

Each time you start working on the project, you should pull the latest changes from the remote repository. This will ensure that you have the most up-to-date version of the code, data, and visualizations.

Run the following command in your terminal to pull the latest changes:

```bash
git pull
```

### 3. Create a new branch

Each time you start working on a new feature or bug fix, you should create a new branch (see [What is a Branch?](#what-is-a-branch)). This will allow you to work on your changes without affecting the main codebase. Once you are done working on your changes, you can submit a pull request to merge your branch into the main codebase.

Run the following command in your terminal to create a new branch:

```bash
git checkout -b <branch-name>
```

Replace `<branch-name>` with a descriptive name for your branch. For example, if you are working on a new feature to add new data about X to the model, you could name your branch `add-data-about-X`.

**If you want to switch to an existing branch, you can run the following command:**

```bash
git checkout <branch-name>
```

Replace `<branch-name>` with the name of the branch you want to switch to.

### 4. Make your changes

Now that you have created a new branch, you can start making your changes. You can use any text editor or IDE to edit the code, data, and visualizations. We recommend using [Visual Studio Code](https://code.visualstudio.com/) and/or [RStudio](https://www.rstudio.com/).

### 5. Commit your changes

Once you are done making your changes, you should commit them to your branch. This will save your changes to your local copy of the repository. (See [What is a commit?](#what-is-a-commit))

Run the following command in your terminal to commit your changes:

```bash
git add -A
git commit -m "<commit-message>"
```

Replace `<commit-message>` with a short description of the changes you made. For example, if you added new data about X to the model, you could use the commit message `Add data about X`.

### 6. Push your changes

Once you have committed your changes, you should push them to the repository. This will save your changes to the remote copy of the repository on GitHub.

Run the following command in your terminal to push your changes:

```bash
git push origin <branch-name>
```

Replace `<branch-name>` with the name of your branch. For example, if you named your branch `add-data-about-X`, you would run the following command:

```bash
git push origin add-data-about-X
```

**To check which branch you are currently working on, run the following command:**

```bash
git branch
```

The branch you are currently on will be highlighted in green.

e.g. `* add-data-about-X`

You can exit the list of branches by pressing `q`.

### 7. Submit a pull request

Once you have pushed your changes to the repository, you should submit a pull request to merge your branch into the main codebase. This will allow others to review your changes before they are merged into the main codebase. (See [What is a pull request?](#what-is-a-pull-request))

To submit a pull request, go to the [Pull requests] tab on GitHub and click the [New pull request] button. Select your branch from the dropdown menu and click the [Create pull request] button. Give your pull request a title and description, and then click the [Create pull request] button again.

Once you have submitted your pull request, you can assign it to someone (probably Asher) to review. Once they have reviewed your changes, they can merge your branch into the main codebase.

## Git FAQs

#### What is Git?

Git is a version control system that tracks the changes in computer files and coordinates work on those files among multiple people. It is primarily used for source code management in software development, but it can be used to keep track of changes in any set of files.

#### What is a Git Repository?

A Git repository is a virtual storage of your project. It allows you to save versions of your code, which you can access when needed.

#### What is GitHub?

GitHub is a cloud-based hosting service that lets you remotely store and manage Git repositories. It is a central location where anyone can share their Git repositories with others.

#### What is a Branch?

A branch is a parallel version of a repository. It is contained within the repository, but does not affect the main branch (or other branches) until it is merged into the main branch. A branch allows you to work on a new feature or bug fix without affecting the main codebase.

#### What is a Commit?

A commit is a saved change to a Git repository. It is like a snapshot of all the files in your project at a particular point in time. You can use commits to keep track of the changes you have made to your project over time. You can also use commits to revert your project back to a previous state. Each commit has a unique ID (e.g. `a4f5b3c`) that allows you to keep track of the changes you have made to your project over time and revert your project back to a previous state.

Each commit also has a commit message that describes the changes made in that commit. For example, if you added new data about X to the model, you could use the commit message `Add data about X`. This will help you keep track of the changes you have made to your project over time and make it easier for others to understand what changes you made in that commit. This is why it is important to write good commit messages!

#### What is a Pull Request?

A pull request is a method of submitting contributions to an open development project. It is a request to merge one set of changes into another set of changes. Typically, a pull request is created by a developer who has made changes to their own branch of a project and wants those changes to be merged into the main branch of the project.
