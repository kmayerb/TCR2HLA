import os
import zipfile
import requests


def download_and_extract_zip( url: str,
                              dest_dir: str = ".",
                              unzip: bool = False,
                              filename: str = "some_new_data.zip"):
    """
    Downloads a ZIP file from a URL and optionally extracts it.

    Args:
        url (str): Direct download URL to the ZIP file.
        dest_dir (str): Directory to download to. Defaults to current directory.
        unzip (bool): Whether to extract the contents. Defaults to True.
        filename (str): Name to save the ZIP file as. Defaults to "data_minimal.zip".

    Returns:
        str: Path to the downloaded (and optionally extracted) files.

    Notes:
    This function was generated wiht ChatGPT using Copilot.
    """
    os.makedirs(dest_dir, exist_ok=True)
    zip_path = os.path.join(dest_dir, filename)

    print(f"📥 Downloading from: {url}")
    response = requests.get(url)
    response.raise_for_status()  # Raise an error if the download fails

    with open(zip_path, "wb") as f:
        f.write(response.content)
    print(f"✅ Downloaded to: {zip_path}")

    if unzip:
        print("📦 Extracting contents (be patient there could be a lot of files to unzip!)...")
        with zipfile.ZipFile(zip_path, 'r') as zip_ref:
            zip_ref.extractall(dest_dir)
        print(f"✅ Extracted to: {dest_dir}")
        return dest_dir
    else:
        return zip_path

data_minimal_url ='https://www.dropbox.com/scl/fi/40lkypwg6xelv5uzedy65/data_minimal_vx.zip?rlkey=5qf2vq36dayk3ca4vdsnkk4di&st=wug8ybjd&dl=1' 

def download_minimal_validation_data(url: str = data_minimal_url, dest_dir: str = ".", unzip: bool = False, filename: str = "TCR2HLA_data.zip"):
    """
    Downloads and extracts the minimal validation data using the predefined data_minimal_url.
    
    Args:
        dest_dir (str): Directory to download to. Defaults to current directory.
        unzip (bool): Whether to extract the contents. Defaults to True.
        filename (str): Name to save the ZIP file as. Defaults to "data_minimal.zip".
    
    Returns:
        str: Path to the downloaded (and optionally extracted) files.
    """
    return download_and_extract_zip(
        url=url,
        dest_dir=dest_dir,
        unzip=unzip,
        filename=filename
    )

"""
# Example usage:
# download_minimal_validation_data(unzip = True, dest_dir = 'TCR2HLA_data',  filename = "TCR2HLA_data.zip")
"""
