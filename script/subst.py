import os
import sys
import subprocess

benchmark = "../benchmarks"

problem = "type"

subst = {
  "let fresh_tyvar _ = (tyvar_num := !tyvar_num + 1; (TyVar (\"t\" ^ string_of_int !tyvar_num)))" :
  "let fresh_tyvar _ = \n\tlet _ = tyvar_num := !tyvar_num + 1 in\n\tTyVar (\"t\" ^ string_of_int !tyvar_num)",
}


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

def replace_code (code):
  result = code
  for key in subst:
    result = result.replace(key, subst[key])

  return result

def execute_command (command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  out = out.decode()
  return (out, err)

def main() :
  path = os.path.dirname(__file__)
  path = os.path.join(path, benchmark)

  year_list = find_dir(os.path.join(path, problem))
  year_list.sort()
  for year in year_list :
    all_submissions = find_files(year)
    all_submissions.sort()
    for submission in all_submissions :
      print(submission)
      f = open(submission, 'r')
      code = f.read()
      new_code = replace_code (code)
      f.close

      print(code)
      print(new_code)
      f = open(submission, 'w')
      f.write(new_code)
      f.close
      new_submission = submission.split(benchmark + "/")[1]
      command = "mv " + submission + " " + new_submission
      print(command)
      execute_command(command)

if __name__ == '__main__':
  main()
