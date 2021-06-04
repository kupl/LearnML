import os
import sys
import subprocess
import numpy as np
import matplotlib.pyplot as plt
from tabulate import tabulate 

######################################################
# All problems represented in the paper's main table #
######################################################
BENCHMARK = [
  (1, "max"), 
  (2, "mem"), 
  (3, "mirror"), 
  (4, "sigma"), 
  (5, "iter"), 
  (6, "uniq"), 
  (7, "nat"), 
  (8, "formula"), 
  (9, "lambda"), 
  (10, "diff")
  ]

BENCHMARK_DIR = "benchmarks"
CORRECT_DIR = os.path.join(BENCHMARK_DIR, "C")
INCORRECT_DIR = os.path.join(BENCHMARK_DIR, "I")
##################################################################
#  The subfunctions for utility. e.g., find directory lists etc. #
##################################################################
# find the all path of subdirectories
def find_dir(path):
  whole_list = os.listdir(path)
  result = []
  for name in whole_list :
    full_path = os.path.join(path,name)
    if(os.path.isdir(full_path)):
      result.append(full_path)
    else:
      continue
  return result

# find the all path of subfiles
def find_files(path):
  whole_list = os.listdir(path)
  result = []
  for f in whole_list :
    result.append(os.path.join(path,f))
  return result

# Executing the given shell commands
def execute_command(command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  out = out.decode()
  return (out, err)

def getLOC(path):
  (out, err) = execute_command("cloc " + path)
  loc = out.split("-------------------------------------------------------------------------------")[2].split(" ")[-1]
  return int(loc)

#####################################
# Construct the Table 1 with result #
#####################################
def construct_table1 (result_dir):
  #No #Problem #I #C LOC Time(FixML) #Fix(FixML) Time(CAFE) #Fix(CAFE)
  result_table = []
  incorrect_total = 0
  correct_total = 0
  loc_total = 0 
  loc_min_total = 1000
  loc_max_total = 0
  time_total_fixml = 0.0
  fix_total_fixml = 0 
  time_total_cafe = 0.0
  fix_total_cafe = 0
  
  cafe_path = os.path.join(result_dir, "cafe")
  fixml_path = os.path.join(result_dir, "fixml")
  
  for (no, problem) in BENCHMARK:
    cafe_problem_path = os.path.join(cafe_path, problem)
    fixml_problem_path = os.path.join(fixml_path, problem)
    
    if os.path.exists(cafe_problem_path) and os.path.exists(fixml_problem_path):
      # Basic information of benchmark set
      incorrect_num = len(find_files(os.path.join(INCORRECT_DIR, problem)))
      correct_num = len(find_files(os.path.join(CORRECT_DIR, problem)))
      locs = list(map(getLOC, find_files(os.path.join(INCORRECT_DIR, problem))))
      loc_sum = sum(locs)
      loc_min = min(locs)
      loc_max = max(locs)

      # Parsing FixML results 
      results_fixml = find_files(fixml_problem_path)
      time_sum_fixml = 0.0
      fix_sum_fixml = 0
      
      for result in results_fixml:
        # Check that if a patch is generated
        with open(os.path.join(result, "result.txt"), "r") as log:
          text = log.read()
          if not ("fails to" in text):
            fix_sum_fixml += 1
            time_sum_fixml += float(text.split(":")[-1].strip("\n"))
      
      # Parsing CAFE results 
      results_cafe = find_files(cafe_problem_path)
      time_sum_cafe = 0.0
      fix_sum_cafe = 0

      for result in results_cafe:
        # Check that if a patch is generated
        with open(os.path.join(result, "result.txt"), "r") as log:
          text = log.read()
          if not ("fails to" in text):
            fix_sum_cafe += 1
            time_sum_cafe += float(text.split(":")[-1].strip("\n"))

      # Update total info 
      incorrect_total += incorrect_num
      correct_total += correct_num
      loc_total += loc_sum
      if loc_min < loc_min_total:
        loc_min_total = loc_min
      if loc_max > loc_max_total:
        loc_max_total = loc_max
      time_total_fixml += time_sum_fixml
      fix_total_fixml += fix_sum_fixml
      time_total_cafe += time_sum_cafe
      fix_total_cafe += fix_sum_cafe

      result_table.append(
        [str(no), 
        problem, 
        str(incorrect_num), 
        str(correct_num), 
        str(round(loc_sum / incorrect_num)) + "(" + str(loc_min) + "-" + str(loc_max) + ")",
        str(time_sum_fixml / fix_sum_fixml),
        str(fix_sum_fixml),
        str(time_sum_cafe / fix_sum_cafe),
        str(fix_sum_cafe)])

  result_table.append(
    [None, 
    "Total", 
    str(incorrect_total),
    str(correct_total), 
    str(round(loc_total / incorrect_total)) + "(" + str(loc_min_total) + "-" + str(loc_max_total) + ")",
    str(time_total_fixml / fix_total_fixml),
    str(fix_total_fixml),
    str(time_total_cafe / fix_total_cafe),
    str(fix_total_cafe)])

  # Construct resulting table
  table = tabulate(result_table, 
    headers=[
      "No", "Problem", "# Wrong", "# Correct", "LOC (min-max)", "Time (FixML)", "#Fix (FixML)", "Time (CAFE)", "#Fix (CAFE)"
    ], 
    tablefmt='orgtbl'
  ) 
  print(table)
  with open (os.path.join(result_dir, "table1.txt"), "w") as table1:
    table1.write(table)
 
###############################################
# Construct the graph in Figure 6 with result #
###############################################
def construct_figure6 (result_dir):
  prog_path = os.path.join(result_dir, "prog")
  func_path = os.path.join(result_dir, "func")
  cafe_path = os.path.join(result_dir, "cafe")
  
  label = []
  cafe = []
  prog = []
  func = []

  for (no, problem) in BENCHMARK:
    prog_problem_path = os.path.join(prog_path, problem)
    func_problem_path = os.path.join(func_path, problem)
    cafe_problem_path = os.path.join(cafe_path, problem)
    
    if os.path.exists(cafe_problem_path) and os.path.exists(func_problem_path) and os.path.exists(prog_problem_path):
      incorrect_num = len(find_files(os.path.join(INCORRECT_DIR, problem)))
      
      # Parsing Prog results 
      results_prog = find_files(prog_problem_path)
      fix_prog = 0
      for result in results_prog:
        with open(os.path.join(result, "result.txt"), "r") as log:
          text = log.read()
          if not ("fails to" in text):
            fix_prog += 1

      # Parsing Func results 
      results_func = find_files(func_problem_path)
      fix_func = 0
      for result in results_func:
        with open(os.path.join(result, "result.txt"), "r") as log:
          text = log.read()
          if not ("fails to" in text):
            fix_func += 1
      
      # Parsing CAFE results 
      results_cafe = find_files(cafe_problem_path)
      fix_cafe = 0

      for result in results_cafe:
        with open(os.path.join(result, "result.txt"), "r") as log:
          text = log.read()
          if not ("fails to" in text):
            fix_cafe += 1

      # Insert new data
      label.insert(len(label), "P"+str(no))
      prog.insert(len(prog), fix_prog/incorrect_num)
      func.insert(len(func), fix_func/incorrect_num)
      cafe.insert(len(cafe), fix_cafe/incorrect_num)

  ### Draw graph
  fig = plt.figure()

  x = np.arange(len(label))
  plt.bar(x-0.2, prog, label="Prog", width=0.2, color="#bc5090", zorder=3)
  plt.bar(x+0.0, func, label="Func", width=0.2, color="#58508d", zorder=3)
  plt.bar(x+0.2, cafe, label="CAFE", width=0.2, color="#003f5c", zorder=3)

  plt.xticks(x, label)

  plt.xlabel('Problem number')
  plt.ylabel('Patch rate (%)')

  plt.legend(bbox_to_anchor=(1.0, 1.0),ncol=3, fancybox=False, facecolor="white")
  plt.rc('axes', axisbelow=True)
  plt.grid(zorder=0, axis="y")
  plt.show()

  fig.savefig(os.path.join(result_dir, "figure6.png"), bbox_inches='tight')


def reproduce(result_dir):
  result_dir = result_dir.strip("/")
  construct_table1(result_dir)
  construct_figure6(result_dir)
  
if __name__ == '__main__':
  reproduce(sys.argv[1])