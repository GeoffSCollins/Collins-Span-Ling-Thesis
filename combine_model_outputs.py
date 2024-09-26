import sys
import os

args = sys.argv[1:]
input_files = args[:-1]
output_filename = args[-1]

first_file = input_files[0]
other_files = input_files[1:]

with open(first_file) as file:
    first_contents = file.readlines()
    
# Replace all the .f with just a .
first_contents = [line.replace(".f", ".").rstrip() for line in first_contents if line.rstrip()]

other_contents = []

# Get the contents of the other files and replace the .f with .
for filename in other_files:
    with open(filename) as file:
        curr_contents = file.readlines()
    
    curr_contents = [line.replace(".f", ".").rstrip() for line in curr_contents if line.rstrip()]
    other_contents.extend(curr_contents)
    
# Get the new lines
new_lines = []
for i, line in enumerate(other_contents):
    # We don't want any fomatting of the table, so we remove it from the line if it is there
    cleaned_line = line.replace("""<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr>""", "")
    
    # Go through each line in first contents to see if there is a match
    found = False
    for orig_line in first_contents:
        if cleaned_line in orig_line:
            found = True
            break

    # Check to see if it was added in the new lines already
    for added_line in new_lines:
        if cleaned_line in added_line:
            found = True
            break
            
    if not found:
        # Add the next 3 lines as well
        new_lines.append(cleaned_line)
        new_lines.append(other_contents[i+1])
        new_lines.append(other_contents[i+2])
        new_lines.append(other_contents[i+3])

# Now that we have the new lines, we split them up by chunks before adding them
chunks = []

curr_chunk = []
for line in new_lines:
    # A new chunk begins when this string is not in the current line
    if """<tr><td style="text-align:left"></td>""" not in line:
        # Don't add an empty chunk at the beginning
        if curr_chunk:
            chunks.append(curr_chunk)
            
        curr_chunk = [line]
    else:
        curr_chunk.append(line)
        
# Don't forget the last one!
chunks.append(curr_chunk)

# We need to place the chunks in the correct locations
def get_chunk_variables(chunk):
    first_line_in_chunk = chunk[0]
    row_name = first_line_in_chunk.replace("""<tr><td style="text-align:left">""", "").split("</td>")[0]
    
    if ":" in row_name:
        variables = row_name.split(":")
        variables = [var.split('.')[0] for var in variables]
        return variables
        
    return [row_name.split(".")[0]]

output = first_contents[:7]

# Go through the first contents (slicing is to ignore the headers and footers) and add
# lines in the other files to get all dependent variables into one table
for line in first_contents[7:-8]:
    if "Dependent variable" not in line:
        row_name = line.replace("""<tr><td style="text-align:left">""", "").split("</td>")[0]
        
        # If it is the start of a row
        if row_name:            
            # If there is only one variable, add all new lines that show only that variable
            if ":" not in row_name:
                row_variable = row_name.split(".")[0]
                
                new_chunks = []
                for chunk in chunks:                    
                    if len(get_chunk_variables(chunk)) == 1:
                        
                        if row_variable == get_chunk_variables(chunk)[0]:
                            output.extend(chunk)
                        else:
                            new_chunks.append(chunk)
                    else:
                        new_chunks.append(chunk)
            
            # If there are multiple variables, add all new lines that show that second variable in the second position
            else:
                second_variable = row_name.split(":")[1].split(".")[0]
                
                # If there is a third variable, add it
                if len(row_name.split(":")) == 3:
                    third_variable = row_name.split(":")[2].split(".")[0]
                else:
                    third_variable = ''
                
                new_chunks = []
                for chunk in chunks:
                    if len(get_chunk_variables(chunk)) == 2:
                        if second_variable == get_chunk_variables(chunk)[1]:
                            output.extend(chunk)
                        else:
                            new_chunks.append(chunk)
                    elif len(get_chunk_variables(chunk)) == 3 and third_variable:
                        if third_variable == get_chunk_variables(chunk)[2]:
                            output.extend(chunk)
                        else:
                            new_chunks.append(chunk)
                    else:
                        new_chunks.append(chunk)

            chunks = new_chunks                    
        output.append(line)

# Add the last 8 lines
output.extend(first_contents[-8:])

if not os.path.exists(os.path.dirname(output_filename)):
    os.makedirs(os.path.dirname(output_filename))
        
# Write the output to a file
with open(output_filename, 'w') as file:
    for line in output:
        file.write(line)
        
# Delete the input files
for file in input_files:
    os.remove(file)