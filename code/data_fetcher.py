from io import TextIOWrapper
from dotenv import load_dotenv
import os
import argparse
from argparse import ArgumentParser, Namespace
import requests
from requests import HTTPError, Response
import csv
import colorama
from colorama import Fore, Style

class DataFetcher:
    """
    A class used to fetch data from an API and write it to the data directory.

    supported filenames:
    - consumer-sentiment.csv

    Attributes
    ----------
    supported_filenames : list[str]

    Methods
    -------
    fetch_and_write_to_data(filename)
        Fetches data from the API and writes it to the data directory.
    """

    def __init__(self) -> None:
        """
        Constructs all the necessary attributes for the DataFetcher object.
        """
        self.files: dict[str, DataFile] = {}
        self.__construct_files_dict()

    def __construct_files_dict(self) -> None:
        self.files['consumer-sentiment.csv'] = DataFile(
            filename='consumer-sentiment.csv',
            api_url='https://api.stlouisfed.org/fred/series/observations',
            request_params={
                'series_id': 'UMCSENT',
                'api_key': FRED_API_KEY,
                'file_type': 'json'
            },
            headers=['date', 'value']
        )
        self.files['inflation-cpi.csv'] = DataFile(
            filename='inflation-cpi.csv',
            api_url='https://api.stlouisfed.org/fred/series/observations',
            request_params={
                'series_id': 'FPCPITOTLZGUSA',
                'api_key': FRED_API_KEY,
                'file_type': 'json'
            },
            headers=['date', 'value']
        )
        self.files['real-gdp-per-capita.csv'] = DataFile(
            filename='real-gdp-per-capita.csv',
            api_url='https://api.stlouisfed.org/fred/series/observations',
            request_params={
                'series_id': 'A939RX0Q048SBEA',
                'api_key': FRED_API_KEY,
                'file_type': 'json'
            },
            headers=['date', 'value']
        )
        self.files['national-unemployment-rate.csv'] = DataFile(
            filename='national-unemployment-rate.csv',
            api_url='https://api.stlouisfed.org/fred/series/observations',
            request_params={
                'series_id': 'UNRATE',
                'api_key': FRED_API_KEY,
                'file_type': 'json'
            },
            headers=['date', 'value']
        )
        self.files['national-home-price-index.csv'] = DataFile(
            filename='national-home-price-index.csv',
            api_url='https://api.stlouisfed.org/fred/series/observations',
            request_params={
                'series_id': 'CSUSHPINSA',
                'api_key': FRED_API_KEY,
                'file_type': 'json'
            },
            headers=['date', 'value']
        )
    
    def fetch_and_write_to_data(self, filename: str) -> None:
        """
        Fetches data from the API and writes it to the data directory.

        Parameters
        ----------
        filename : str
            The filename of the data to update.

        Raises
        ------
        ValueError
            If the filename is invalid.
        HTTPError
            If the HTTP request returns an error.
        """
        try:
            data_file: DataFile = self.files[filename]
            data_file.handle_request()
        except KeyError:
            raise ValueError(f'Invalid filename: {filename}')
    
    def get_supported_filenames(self) -> list[str]:
        """
        Returns the supported filenames.

        Returns
        -------
        list[str]
            The supported filenames.
        """
        return self.files.keys()
    
class DataFile:
    """
    A class used to represent a data file.

    Attributes
    ----------
    filename : str
    api_url : str
    request_params : dict[str, str]
    headers : list[str]
    """

    def __init__(self, filename: str, api_url: str, request_params: dict[str, str], headers: list[str]) -> None:
        self.filename: str = filename
        self.api_url: str = api_url
        self.request_params: dict[str, str] = request_params
        self.headers: list[str] = headers

    def handle_request(self) -> None:
        """
        Handles the HTTP request to the API.

        Raises
        ------
        HTTPError
            If the HTTP request returns an error.
        """
        response: Response = requests.get(self.api_url, params=self.request_params)
        if response.status_code == 200:
            response_json: dict[str, str] = response.json()
            observations: list[dict[str, str]] = response_json['observations']
            filepath: str = f'data/{self.filename}'
            file: TextIOWrapper = open(filepath, 'w', newline='')
            writer = csv.writer(file)
            writer.writerow(self.headers)
            for observation in observations:
                writer.writerow([observation[header] for header in self.headers])
            file.close()
        else:
            raise HTTPError(f'HTTP Error: {response.status_code} {response.reason} (request url: {response.request.url})')

if __name__ == '__main__':
    # Load the environment variables
    load_dotenv()
    FRED_API_KEY = os.getenv('FRED_API_KEY')

    # Create the parser
    parser: ArgumentParser = argparse.ArgumentParser(description='Process some arguments.')

    # Add the arguments
    parser.add_argument('--filename', type=str, help='The filename of the data to update.')
    parser.add_argument('--all', action='store_true', help='Update all files.')

    # Parse the arguments
    args: Namespace = parser.parse_args()

    # Get the filename from the arguments
    filename: str = args.filename
    update_all: bool = args.all

    if not update_all and filename is None:
        print('No filename provided. Please provide a filename, or use the --all flag to update all files.')
        exit(1)
    
    if update_all:
        # Update all files
        data_fetcher: DataFetcher = DataFetcher()
        supported_filenames: list[str] = data_fetcher.get_supported_filenames()
        max_filename_length: int = max([len(filename) for filename in supported_filenames])
        for filename in supported_filenames:
            length_difference: int = max_filename_length - len(filename)
            try:
                data_fetcher.fetch_and_write_to_data(filename)
            except ValueError as e:
                print(f"[{Fore.RED}{filename}{Style.RESET_ALL}] {' ' * length_difference}{e}")
            except HTTPError as e:
                print(f"[{Fore.RED}{filename}{Style.RESET_ALL}] {' ' * length_difference}{e}")
            else:
                print(f"[{Fore.GREEN}{filename}{Style.RESET_ALL}] {' ' * length_difference}Updated {filename} successfully!")
        exit(0)

    # Get the data from the API
    data_fetcher: DataFetcher = DataFetcher()
    try:
        data_fetcher.fetch_and_write_to_data(filename)
    except ValueError as e:
        print(e)
        exit(1)
    except HTTPError as e:
        print(e)
        exit(1)
    else:
        print(f'Updated {filename} successfully!')
        exit(0)