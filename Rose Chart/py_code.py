import numpy as np 
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
dat = pd.read_excel('Dat.xlsx')
dat


dat['period'].value_counts()

dat1 = dat[dat['period'] == 'April 1854 to March 1855']
dat2 = dat[dat['period'] == 'April 1855 to March 1856']
dat1.head()
dat2.head()
Levels = ['Disease', 'Wounds', 'Other']
Time = ['April 1854 to March 1855', 'April 1855 to March 1856']
widths = np.radians(360/12)
directions = np.linspace(7.5, 350, num = 12)
Labels = list(dat['Month'].unique())
Labels
colors = ['darkblue', 'gold', 'red']


fig, ax = plt.subplots(1, 2, figsize = (8, 8), subplot_kw = dict(polar = True))
count = -1
for k in Time: 
    # k = Time[0]
    count += 1
    temp1 = dat[dat['period'] == k]
    Disease = temp1[temp1['Cause'] == Levels[0]]['Rate'].values
    Wounds = temp1[temp1['Cause'] == Levels[1]]['Rate'].values
    Other = temp1[temp1['Cause'] == Levels[2]]['Rate'].values
    ax[count].set_yticklabels([])
    ax[count].bar(np.radians(directions), Other, 
                color = colors[2], width = widths, 
                label = 'Other')
    ax[count].bar(np.radians(directions), Wounds, 
                color = colors[1], width = widths, 
                bottom = Other, label = 'Wounds')
    ax[count].bar(np.radians(directions), Disease, 
               color = colors[0], width = widths, 
                bottom = Wounds + Other,
                label = 'Disease')
    ax[count].set_xticks(np.radians(directions))
    ax[count].set_xticklabels(Labels)
    ax[count].set_rlabel_position(0) 
    ax[count].set_title(k)
    ax[count].legend() 

img = mpimg.imread('my_arm.png')
ax_img = fig.add_axes([0.2, 0.85, 0.6, 0.05], zorder = 10) 
ax_img.imshow(img)
ax_img.axis('off')
plt.show()
