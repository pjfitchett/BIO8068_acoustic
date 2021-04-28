# Visualisation and analysis of wildlife acoustics

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)

# Southeast Asian monkey recordings ####

# Create a folder called FocalRecordings
dir.create(file.path("FocalRecordings"), showWarnings = FALSE)

# Load the sound files in the behaviouR package
githubURL <- "https://github.com/DenaJGibbon/behaviouRdata/raw/master/data/FocalRecordings.rda"
FocalRecordings <- get(load(url(githubURL)))

# Save the recordings in the new FocalRecordings folder
for (a in 1:length(FocalRecordings)) {
  FileName <- FocalRecordings[[a]][1][[1]]
  WaveFile <- FocalRecordings[[a]][2][[1]]
  writeWave(WaveFile, paste("FocalRecordings/", FileName, sep = 
                              ""))
}

# Importing and displaying an individual .wav file
GibbonWaveFile <- readWave("FocalRecordings/FemaleGibbon_1.wav")
GibbonWaveFile 
# Shows file is 13 secs long, recorded at a sampling rate of 44100 samples/sec
# and a total of 572054 samples

# Can check the duration makes sense in terms of the total number of records
duration(GibbonWaveFile) * GibbonWaveFile@samp.rate

# Plot the amplitude using the oscillo function 
# Oscillations reverberate around the central line
oscillo(GibbonWaveFile)
# Larger amplitude = louder sound 

# Zoom into a fraction of a second to display the waveform more clearly 
oscillo(GibbonWaveFile, from = 0.1, to = 0.2)
# Still difficult to see - zoom in further
oscillo(GibbonWaveFile, from = 0.15, to = 0.2)

# Creating a spectrogram ####

# Shows how the spectrum of frequencies varies over time
# Shows the amplitude in different colours to keep the plot 2D 

SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav")
# Shows very little info above 3kHz - possible background noise?

# Female gibbon calls are at lower frequencies - zoom to this
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500)

# Display in colour - easier to see
SpectrogramSingle(sound.file = "FocalRecordings/FemaleGibbon_1.wav", min.freq = 500, 
                  max.freq = 2500, Colors = "Colors")

# Can create a ggplot compatible spectrogram using ggspectro

# Trial and error to get limits and spectro.colors at a suitable scale
v <- ggspectro(GibbonWaveFile, flim=c(0,2.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)
v

# Displaying multiple spectrograms ####

# Easier to make comparisons

# We can tell R to print the spectrograms 2x2 using the code below
par(mfrow = c(2, 2))

# This is the function to create the spectrograms
SpectrogramFunction(input.dir = "FocalRecordings", min.freq = 500, max.freq = 2500,
                    Colors = "Colors")
par(mfrow = c(1,1))

# Simplifying audio data to allow multivariate analysis ####

# MFCC - looks for repeated patterns in the sounds and undertakes feature extraction 
# Simplifies to a simpler form
FeatureDataframe <- MFCCFunction(input.dir = "FocalRecordings")
dim(FeatureDataframe)
View(FeatureDataframe)

# Now analyse the dataframe using PCA
library(vegan)
source("nes8010.R")

# Use [,-1] to keep all rows but omit first column
acoustics_pca <- ordi_pca(FeatureDataframe[, -1], scale=TRUE)
summary(acoustics_pca)
# PC1 explains 45.75%, PC2 explains 18.64% of the variation

# Can now visualise the individual sound records (sites/rows)
# Do not need to visualise spp/columns
dev.off() # to fix error message about invalid graphics state
ordi_plot(acoustics_pca, display="sites")

# Redraw the graph by extracting scores and using ggplot labelling
acoustics_sco <- ordi_scores(acoustics_pca, display="sites")
acoustics_sco <- mutate(acoustics_sco, group_code = FeatureDataframe$Class)

ggplot(acoustics_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()
# Can clearly see the three groups of monkeys are different to each other

# Songbird analysis ####
library(warbleR)

# Blackbird Turdus merula in UK restricted to 5-25 secs 
blackbird_songs <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:song len:5-25',
                            download = FALSE) # not actually downloading the audio

blackbird_alarm <- query_xc(qword = 'Turdus merula cnt:"united kingdom" type:alarm len:5-25', 
                            download = FALSE)

# Create interactive leaflet maps with popups for locations of recordings
map_xc(blackbird_songs, leaflet.map = TRUE)

# Create subfolders for song calls and alarm calls
dir.create(file.path("blackbird_songs"))
dir.create(file.path("blackbird_alarm"))

# Download MP3 files into two separate sub-folders
query_xc(X = blackbird_songs, path="blackbird_songs")
query_xc(X = blackbird_alarm, path="blackbird_alarm")
# Some files are better quality than others 

library(stringr)

# List of all file names in the blackbird_songs subfolder
# Create an empty R object called new_files
# Loop to repeat a set of commands for each file
old_files <- list.files("blackbird_songs", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)
# ^ can be used for different types of call or bird spp 

# Minor changes for alarm calls
old_files <- list.files("blackbird_alarm", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-alarm_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# Renamed all original files, now copy them to a new subfolder called blackbird_audio
dir.create(file.path("blackbird_audio"))
file.copy(from=paste0("blackbird_songs/",list.files("blackbird_songs")),
          to="blackbird_audio")
file.copy(from=paste0("blackbird_alarm/",list.files("blackbird_alarm")),
          to="blackbird_audio")

# Convert MP3 to WAV files then remove MP3 files
mp32wav(path="blackbird_audio", dest.path="blackbird_audio")
unwanted_mp3 <- dir(path="blackbird_audio", pattern="*.mp3")
file.remove(paste0("blackbird_audio/", unwanted_mp3))

# Visualise and analyse the song and alarm calls ####

# Start with a single bird
blackbird_wav <- readWave("blackbird_audio/Turdusmerula-song_243908.wav")
blackbird_wav

# Plot the frequency diagram
oscillo(blackbird_wav)

oscillo(blackbird_wav, from = 0.59, to = 0.60)

SpectrogramSingle(sound.file = "blackbird_audio/Turdusmerula-song_243908.wav",
                  Colors = "Colors")

# MFCC of blackbird song and alarm calls
# Need to change max frequency to 7000 Hz 
blackbird_mfcc <- MFCCFunction(input.dir = "blackbird_audio",
                               max.freq=7000)
dim(blackbird_mfcc)

# PCA
blackbird_pca <- ordi_pca(blackbird_mfcc[, -1], scale=TRUE)
summary(blackbird_pca)
# PC1 explains 17.42%, PC2 explains 12.76%
# Lower explanation possibly due to the fact it is citizen science data
# Also not filtered based on quality of audio

blackbird_sco <- ordi_scores(blackbird_pca, display="sites")
blackbird_sco <- mutate(blackbird_sco, group_code = blackbird_mfcc$Class)

ggplot(blackbird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 
# Large overlap between alarm and song calls 
# Some audios described as alarm are variable 
# Can still see a separation between the calls, most songs on LR, most alarms on UL



