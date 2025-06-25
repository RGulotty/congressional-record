install.packages("remotes")
install.packages("quanteda")
install.packages("devtools")

library(quanteda)
library(conText)
library(data.table)
library(data.table)

# Read all lines
lines <- readLines("data_raw/hein-bound/speeches_043.txt", encoding = "UTF-8", warn = FALSE)

# Header
colnames <- strsplit(lines[1], split = "|", fixed = TRUE)[[1]]

# Body (remove first line)
lines_body <- lines[-1]

# Encoding fix (optional but recommended for OCR'd text)
lines_body <- iconv(lines_body, from = "", to = "UTF-8", sub = "byte")

# Vectorized splitting: first field and the rest joined
split_dt <- tstrsplit(lines_body, split = "|", fixed = TRUE, keep = 1:2)

# For extra pipes in the text, recombine all but the first field
speech_id <- split_dt[[1]]
text <- sub("^[^|]*\\|", "", lines_body)  # removes everything up to the first |

# Build the final data.table
dt <- data.table(speech_id = speech_id, text = text)

# Apply original column names
setnames(dt, colnames)


# Create corpus
corp <- corpus(dt, text_field = "speech")

# Add docvars for grouping
#docvars(corp, "party") <- cr_data$party
#docvars(corp, "year") <- cr_data$year

# Create tokens (basic preprocessing)
toks <- tokens(corp, 
               remove_punct = TRUE,
               remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en"))

# Construct feature matrix (optional filtering)
dfm_mat <- dfm(toks)
dfm_trimmed <- dfm_trim(dfm_mat, min_termfreq = 10)

# Fit embeddings stratified by a covariate (e.g., party)
# get context words sorrounding the term immigration
context_immigration <- get_context(x = cr_sample_corpus, target = 'immigration',
                                   window = 6, valuetype = "fixed", case_insensitive = FALSE,
                                   hard_cut = FALSE, verbose = FALSE)

# Compute semantic shift (e.g., between Democrats and Republicans)
shift <- compute_s_values(
  embeddings = embed,
  target = "climate",   # or any target term
  covariate_values = c("Democrat", "Republican")
)

# View results
print(shift)