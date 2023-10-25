import re
import string
import nltk
from nltk.stem import WordNetLemmatizer
import spacy
import os


# nltk.download("punkt")
# nltk.download("wordnet")
nlp = spacy.load("en_core_web_sm")
nlp.max_length = 1500000


def clear_folder_contents(folder_path):
    # Verify that the folder exists before attempting to clear it
    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        for filename in os.listdir(folder_path):
            file_path = os.path.join(folder_path, filename)
            try:
                if os.path.isfile(file_path):
                    os.remove(file_path)
                elif os.path.isdir(file_path):
                    os.rmdir(file_path)
                print("Deleted", file_path, "lmao")
            except Exception as e:
                print(f"Failed to delete {file_path}: {e}")
    else:
        os.makedirs(folder_path)
        # print(f"The folder '{folder_path}' does not exist.")


def year_of_filename(filename):
    """takes a filename from the folder "text" and provides the year

    Args:
        filename (string): the filename to be analyzed
    """
    components = filename.split("_")
    if len(components) == 3:
        prefix, identifier, extension = components
        # print("Prefix:", prefix)
        # print("Identifier:", identifier)
        # print("Extension:", extension)
    else:
        print("The filename does not have the expected format.")

    return int(identifier)


# def lemmatize_text(input_text):
#     # Tokenize the text into words
#     words = nltk.word_tokenize(input_text)

#     # Initialize the lemmatizer
#     lemmatizer = WordNetLemmatizer()

#     # Lemmatize each word in the list
#     lemmatized_words = [lemmatizer.lemmatize(word) for word in words]

#     # Join the lemmatized words back into a string
#     lemmatized_text = " ".join(lemmatized_words)

#     return lemmatized_text


def lemmatize_text(text):
    doc = nlp(text)
    lemmatized_text = " ".join(token.lemma_ for token in doc)
    return lemmatized_text


def contents_from_filepath(filepath):
    try:
        with open(filepath, "r") as file:
            lines = file.readlines()
            if len(lines) >= 3:
                # removes the first 2 lines
                text = lines[2]
                # no punctuation
                text = text.translate(str.maketrans("", "", string.punctuation))
                # no numbers
                text = re.sub(r"\d", "", text)
                # lowercase
                text = text.lower()
                # lemmatizes with spacy
                text = lemmatize_text(text)
                return text
            else:
                return "The file does not have at least three lines."
    except FileNotFoundError:
        return "File not found."


def append_text_to_file(new_filename, old_filename, text_to_append):
    try:
        with open(new_filename, "a") as file:
            file.write(text_to_append)
        print("Text appended successfully from", old_filename, "to", new_filename)
    except FileNotFoundError:
        print("File not found.")


def year_to_decade(year):
    if year < 0:
        return "Invalid year"

    decade = (year // 10) * 10
    return decade


def year_to_millennium(year):
    if year < 0:
        return "Invalid year"

    millennium = (year // 1000) * 1000
    return millennium


folder_path = "text/"  # Replace with the path to your folder


def write_to_decades():
    # Check if the folder exists
    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        # List all files in the folder
        files = os.listdir(folder_path)

        for file_name in files:
            file_path = os.path.join(folder_path, file_name)

            # Check if the item in the folder is a file (not a directory)
            if os.path.isfile(file_path):
                filename = os.path.basename(file_path)
                year = year_of_filename(filename)
                decade = year_to_decade(year)
                new_path = "corpora_decade/" + str(decade) + ".txt"
                append_text_to_file(
                    new_path, filename, contents_from_filepath(file_path)
                )
            else:
                print("The specified folder does not exist.")


def write_to_millenia():
    # need to delete old contents to ensure we don't append onto old stuff
    clear_folder_contents("corpora_millennium")
    # Check if the folder exists
    if os.path.exists(folder_path) and os.path.isdir(folder_path):
        # List all files in the folder
        files = os.listdir(folder_path)
        for file_name in files:
            file_path = os.path.join(folder_path, file_name)
            # Check if the item in the folder is a file (not a directory)
            if os.path.isfile(file_path):
                filename = os.path.basename(file_path)
                year = year_of_filename(filename)
                # choosing which folder to write to
                millennium = 1800 if year < 1900 else 1900
                new_path = "corpora_millennium/" + str(millennium) + ".txt"
                append_text_to_file(
                    new_path, filename, contents_from_filepath(file_path)
                )
            else:
                print("The specified folder does not exist.")


write_to_millenia()
