import os
import sys
import subprocess

'''
  ("KoreaUniv_maxmin","max","","max"),
  ("KoreaUniv_mirror","mirror","","mirror"),
  ("KoreaUniv_mem","mem","","mem"),
  ("crazy2add","grading","grading.ml","crazy2add"),
  ("formula","eval","","formula1"),
  ("KoreaUniv_formula_16","eval","","formula2"),
  ("checkMetro","checkMetro","","checkMetro"),
  ("KoreaUniv_wellformedness","check","","lambda"),
  ("KoreaUniv_lambda","check","","lambda"),
  ("diff","grading","grading.ml","diff1"),
  ("KoreaUniv_diff","grading","grading.ml","diff2"),
'''
benchmark_info = [
  #problem_path, entry, grading, generator
  ("diff","grading","grading.ml","diff1"),
]

native = "qcheck.native" 
  
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

def find_files(path):
  whole_list = os.listdir(path)
  result = []
  for f in whole_list :
    result.append(os.path.join(path,f))
  return result

def gen_command (entry, solution, submission, generator, grading):
  engine = "./" + native + " -qcheck"
  opt_solution = " -solution " + solution
  opt_entry = " -entry " + entry
  opt_submission = " -submission " + submission
  opt_modulespec = " -external ./engine/moduleSpec.ml"
  opt_generator = " -generator " + generator
  if(grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading " + grading

  command = engine + opt_solution + opt_entry + opt_submission + opt_modulespec + opt_generator + opt_grading
  return command

def execute_command (command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  if not err :
    return out.decode()
  else :
    return ("Execution Failure")
  
def run_qcheck (entry, solution, submission, generator, grading):
  command = gen_command (entry, solution, submission, generator, grading)
  print(command)
  out = execute_command (command)
  print(out)
  return out

def main() :
  f=open("qcheck.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks2')
  
  for (hw, entry, grading, generator) in benchmark_info :
    hw_path = os.path.join (path, hw)
    solution = os.path.join (hw_path, "sol.ml")
    if (grading != ""):
      grading = os.path.join (hw_path, grading)

    year_list = find_dir(os.path.join(path,hw))
    year_list.sort()
    for year in year_list :
      submissions = find_files(year)
      submissions.sort()
      for submission in submissions:
        out = run_qcheck (entry, solution, submission, generator, grading)
        f.write(submission+"\n")
        f.write(out+"\n")

  f.close()   

if __name__ == '__main__':
  main()
