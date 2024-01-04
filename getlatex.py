import time
import pyperclip
from PIL import ImageGrab
from pix2tex.cli import LatexOCR
#pip3 install torch pyperclip pillow "pix2tex[gui]" 
model = LatexOCR()
while True:
    time.sleep(0.5)
    img = ImageGrab.grabclipboard()
    if img != None:
        text = str(model(img))
        # pyperclip.copy('\\begin{align}' + text + '\\end{align}')
        pyperclip.copy(text)
        print(text)