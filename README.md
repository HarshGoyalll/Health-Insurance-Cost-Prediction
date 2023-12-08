# Health Insurance Cost Prediction

This project aims to predict health insurance costs using a machine learning model trained on a dataset that includes various factors such as age, sex, BMI, number of children, smoking habits, and region. The project also includes a web application that provides a user-friendly interface to make predictions based on the trained model.

## Introduction

The main objective of this project is to leverage the power of machine learning to accurately predict health insurance costs. By training a machine learning model on a dataset that contains relevant features, we can estimate insurance costs based on individual attributes. The model takes into account factors such as age, sex, BMI, number of children, smoking habits, and region to generate accurate predictions.

To make the prediction process accessible to users, we have developed a web application. The application provides a simple and intuitive interface where users can input their information and obtain an estimated insurance cost. The underlying machine learning model handles the prediction calculations, ensuring reliable results.

## How to Use

To use the Health Insurance Cost Prediction web application, follow these steps:

1. Visit the web application by clicking on the following link: [Health Insurance Cost Prediction](https://healthinsurancepred.streamlit.app/).

2. Once on the web application, you will see a user interface where you can enter the necessary information for the prediction.

3. Fill in the required fields:
   - **Age:** Slide the bar to select your age.
   - **Sex:** Choose your gender from the dropdown menu.
   - **BMI Value:** Enter your Body Mass Index (BMI) value.
   - **Number of Children:** Slide the bar to select the number of children you have.
   - **Smoker:** Select "Yes" if you are a smoker; otherwise, select "No."
   - **Region:** Choose your region from the dropdown menu.

4. After entering the information, click on the "Predict" button.

5. The application will process your input and display the predicted insurance amount based on the provided data.

Through the web application, users can conveniently obtain estimated health insurance costs by leveraging the power of machine learning.

## Dataset and Model

The web application utilizes a machine learning model that has been trained on the "insurance.csv" dataset. This dataset, located in the "dataset" folder of this repository, contains information about individuals' health insurance costs. By analyzing the patterns and relationships in the dataset, the machine learning model can make accurate predictions based on new input data.

The trained machine learning model is stored in the "insurance_pred" file, located in the "ML Model" folder. This model has been specifically developed and trained to handle health insurance cost prediction based on the provided dataset.

## Repository Structure

This repository has the following structure:

```
├── App
│   └── app.py
├── dataset
│   └── insurance.csv
├── ML Model
│   └── insurance_pred
├── README.md
└── requirements.txt
```

- `App/app.py`: The main script that runs the web application.
- `dataset/insurance.csv`: The dataset used for training the machine learning model.
- `ML Model/insurance_pred`: The trained machine learning model.
- `README.md`: The file you are currently reading.
- `requirements.txt`: The list of dependencies required to run the web application.

Feel free to explore the code and modify it according to your needs. If you encounter any issues or have suggestions for improvement, please create an issue on the repository.

## Prerequisites

To run this web application locally, you need to have the following dependencies installed:

- joblib==1.2.0
- streamlit==1.23.1
- scikit-learn
- pillow

You can install these dependencies by running the following command:

```bash
pip install -r requirements.txt
```

## License

This project is licensed under the [MIT License](LICENSE).
```

Please verify if the updated content meets your requirements.
