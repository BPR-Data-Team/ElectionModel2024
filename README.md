# [24cast.org](https://24cast.org) Model Repository

### The complete collection of BPR's code, data, and visualizations for our 2024 election model. 

*If you're looking for the codebase for our website, [click here](https://github.com/BPR-Data-Team/ElectionModel2024-site).*

---

See [Git Guide.md](GitGuide.md) for instructions on how to clone the repository and contribute to the project

See the [Data](data) folder for all _non-cleaned_ data. All of this data has a _cleaned counterpart_ in [Cleaned Data](cleaned_data).

See the [Code](code) folder for all cleaning, scraping, and model code.

See [Data Citations.md](DataCitations.md) for disclosures of authorship on the data we use.

[Archive](archive) contains old code and models that are defunct.

Check out our [GitHub Workflows](.github/workflows) to see how we update our data daily and subsequently create new predictions.

---

<details>
  <summary>See how to install a virtual environment in the root directory of the project for working on Python files</summary>

  ### 1. From the root directory of the project, run the following command in your terminal to install a virtual environment:
  
  ```bash
  python3 -m venv venv
  ```
  
  This will create a new folder called `venv` in the root directory of the project. This folder will contain all of the dependencies for the project. This is a one-time step. You will not need to install a virtual environment again. This folder is ignored by Git, so it will not be tracked by the repository.
  
  ### 2. Activate the virtual environment:
  
  ```bash
  source venv/bin/activate
  ```
  
  Do this each time you start working on the project. You will know that the virtual environment is active when you see `(venv)` at the beginning of your terminal prompt. You can deactivate the virtual environment at any time by running the following command:
  
  ```bash
  deactivate
  ```
  
  ### 3. Install the project's dependencies:
  
  ```bash
  pip install -r requirements.txt
  ```
</details> 

<details>
  <summary>See how to create an .env file with the required API keys if you want to run data_fetcher.py</summary>
  
  ### 1. Create a copy of the .env.example file and rename it to .env:

  ```bash
  cp .env.example .env
  ```
  
  ### 2. Fill in the required API keys in the .env file:
  
  ```bash
  # .env
  FRED_API_KEY=your_fred_api_key
  ```
  
  You can either get your own API keys or ask a member of the team for the keys.
</details>
