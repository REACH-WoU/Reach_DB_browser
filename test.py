import pandas as pd

full_file = pd.read_excel('excel_input.xlsx')

columns = full_file.columns
columns = ['"' + column + '"' for column in columns]
print('c(' + ', '.join(columns) + ')')
