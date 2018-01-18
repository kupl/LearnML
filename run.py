import os
import sys
import subprocess

#test_folders = ["iter","diff","eval","formula","zipper","zipperN"]

#test_folders = ["iter","diff","eval","formula","sigma","zipper","zipperN","crazy2add"]
test_folders=["sigma"]

fail_count = 0
succ_count = 0


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

def gen_command(submission,path,func):
  opt_testcases = " -testcases "+(os.path.join(path,'testcases'))
  opt_solution = " -solution "+(os.path.join(path,'sol.ml'))
  opt_entryfunction = " -entry "+(func)
  opt_submission = " -submission "+submission
  opt_modulespec = " -external engine/moduleSpec.ml"
  command = "engine/main.native -fix"+opt_submission+opt_modulespec+opt_solution+opt_entryfunction+opt_testcases
  return command

def execute(submission,path,func):
  command = gen_command(submission,path,func)
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  global fail_count
  global succ_count
  #print(command)
  if not err:
    #print(command)
    if(out.find("None")==-1):
      succ_count=succ_count+1
    else:
      fail_count=fail_count+1
    print(out)
    return True
  else:
    #print(command)
    #print(err)
    return False

def main() :
  path = os.path.dirname(__file__)
  path = os.path.join(path,'benchmarks')
  commands = []
  for hw in test_folders :
    count=0
    err_count=0
    dir_list = find_dir(os.path.join(path,hw))
    for year in dir_list :
      submissions = find_files(year)
      for submission in submissions:
        count=count+1
        b=execute(submission,os.path.join(path,hw),hw)
        if b is False:
          err_count = err_count+1
        else:
          pass
    print(hw)
    print("Submissions: ",count)
    print("Success: ",succ_count)
    print("Fail: ",fail_count)

if __name__ == '__main__':
  main()
