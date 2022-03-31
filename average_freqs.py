# -*- coding: utf-8 -*-
"""average_freqs.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1QaYtbrqosaUgtkQN4Ftk3lGVBicAPhix

This is the code for getting a mean frequencies of given .wav files.
After we got the frequency for each .wav file, then it will export 'audio_data.csv' which is a table and its columns are filename of .wav files and corresponding frequencies.
"""

pip install pydub

import os
import numpy as np
import pydub
from os import path
from pydub import AudioSegment
import pandas as pd

table = {'file_name':[], 'average_frequencies':[]}

import zipfile
with zipfile.ZipFile('/content/sample.zip', 'r') as zip_ref:
    zip_ref.extractall('/content')

def read(f, normalized=False):
    """MP3 to numpy array"""
    a = pydub.AudioSegment.from_mp3(f)
    y = np.array(a.get_array_of_samples())
    if a.channels == 2:
        y = y.reshape((-1, 2))
    if normalized:
        return a.frame_rate, np.float32(y) / 2**15
    else:
        return a.frame_rate, y

def get_mean_freqs(y: np.ndarray, fs: int) -> int:
    spec = np.abs(np.fft.rfft(y))
    freq = np.fft.rfftfreq(len(y), d=1 / fs)
    spec = np.abs(spec)
    amp = spec / spec.sum()
    mean = (freq * amp).sum()
    return mean

for file_name in os.listdir('/content/sample'):
  input_path = '/content/sample/' + file_name
  average_freq = get_mean_freqs(read(input_path)[1], 44100)
  table['file_name'].append(file_name)
  table['average_frequencies'].append(average_freq)

table = pd.DataFrame(table)
table.to_csv('audio_data.csv')