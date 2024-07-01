# kelpnet
repository for KelpNet model, which is based on CoralNet. Using transfer learning to adapt the existing CNN for kelp images to improve accuracy with classification of ROV images.

## project setup
Create conda environment with python >= 3.11 (project was created with 3.11, no guarantees made for other versions of python).
```
conda create -n kelpnet python=3.11
conda activate kelpnet
pip install -r /path/to/requirements.txt
```

## download data patches from CoralNet website from your choice data source
I am working with 'CPP_ROV', so I downloaded the confirmed annotated patches from there.

## recommendations
Make a copy of the Jupyter Notebook (.ipynb file), so you can mess around with one and keep one intact.
