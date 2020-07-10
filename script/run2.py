import os
import sys
import subprocess

benchmark_info = [
  ("diff_All","grading","grading.ml","testcases/diff_testcases"),
]

native = "./engine/main.native" 

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

def gen_command(submission,path,entry,grading,specs):
  opt_testcases = " -testcases "+(os.path.join(path, specs))
  opt_solution = " -solution "+(os.path.join(path,'sol.ml'))
  opt_entryfunction = " -entry "+entry
  opt_submission = " -submission "+submission
  opt_modulespec = " -external engine/moduleSpec.ml"
  if(grading == ""):
    opt_grading = ""
  else:
    opt_grading = " -grading "+(os.path.join(path,grading))
  
  if(sys.argv[1]=="run"):
    running_option = "./"+native+" -run"
  elif(sys.argv[1]=="fix"):
    running_option = "./"+native+" -fix"
  elif(sys.argv[1]=="fix2"):
    running_option = "./"+native+" -fix"
    opt_testcases = ""
  elif(sys.argv[1]=="eq"):
    running_option = "./"+native+" -eq"
  elif(sys.argv[1]=="gen_test"):
    running_option = "./"+native+" -gentest"
    opt_testcases = ""
  elif(sys.argv[1]=="exp_cover"):
    running_option = "./"+native+" -exp_cover"
  else:
    print("mode : fix || run || eq")
    sys.exit(0)

  command = running_option+opt_submission+opt_modulespec+opt_solution+opt_entryfunction+opt_grading+opt_testcases
  return command

def execute_command (command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  out = out.decode()
  return (out, err)

def execute(submission,path,entry,grading,specs):
  command = gen_command(submission,path,entry,grading,specs)
  (out, err) = execute_command (command)

  if not err:
    if(sys.argv[1]=="run"):
      print(str(submission).replace(str(path+"/"),""))
      score = out.split("score : ")[1]
      if score in score_result:
        score_result[score] = score_result[score]+1
      else:
        score_result[score] = 1

    elif(sys.argv[1]=="fix"):
      if (out.find("The submission is correct code")>=0):
        pass
      elif (out.find("None") == -1):
        try_count=try_count+1
        succ_count=succ_count+1
        print(submission + " ")
        print(out)
        f.write(submission + " ")
        f.write(out)
      else:
        try_count=try_count+1
        fail_count=fail_count+1
        print(submission + " ")
        print(out)
        f.write(submission + " ")
        f.write(out)

    elif(sys.argv[1]=="eq"):
      print(submission + " ")
      print(out)
      if (False):
        pass
      else:
        f.write(submission + " ")
        f.write(out)

    elif(sys.argv[1]=="gen"):
      print(submission + " ")
      print(out)
      f.write(submission + " ")
      f.write(out)

    elif(sys.argv[1]=="exp_cover"):
      print(submission)
      total = out.split("Total Exp : ")[1]
      cover = total.split("Covered Exp : ")[1]
      total = int(total.split("\n")[0])
      cover = int(cover.split("\n")[0])
      uncover = total-cover
      print(total, cover, uncover)

    else:
      print("mode : fix || run")
      sys.exit(0)

    return True
   
  else:
    return False

def run_testcase (submission, path, entry, grading, specs) :
  command = gen_command (submission, path, entry, grading, specs)
  print(command)
  (out, err) = execute_command (command)
  if out.find("score : ") != -1: 
    score = out.split("score : ")[1]
    return (True, score)
  else:
    return (False, 0)

def run_module () :
  f=open("grading.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks')

  for (hw,entry,grading,specs) in benchmark_info :
    hw_path = os.path.join (path, hw)

    error_num = 0
    fail_num = 0
    score_result = {}

    total_score = (sum(1 for line in open(path + "/" + hw + "/" + specs)) - 2)

    year_list = find_dir(os.path.join(path,hw))
    year_list.sort()
    for year in year_list :
      submissions = find_files(year)
      submissions.sort()
      for submission in submissions:
        command = gen_command (submission, path, entry, grading, specs)
        (succ, score) = run_testcase (submission, hw_path, entry, grading, specs)
        if (succ) :
          error_num += 1
          if score in score_result:
            score_result[score] = score_result[score]+1
          else:
            score_result[score] = 1

          print (submission)
          print ("Score : " + score)
          f.write(submission + "- Score : " + score + "\n")
        else :
          fail_num += 1
          print (submission)
          print ("Non logical Error")
          f.write(submission + "\n")
          f.write("Non logical Error\n")

    
    benchmark_total = error_num + fail_num
    print("Num of Benchmarks : " + str(benchmark_total))
    print("Num of Error : " + str(error_num))
    f.write("===========================================\n")
    f.write("================" + hw + "===============\n")
    f.write("Num of Benchmarks : " + str(benchmark_total) + "\n")
    f.write("Num of Error : " + str(error_num) + "\n")
    f.write("===========================================\n")
  
  f.close()

def check_exp_cover (submission, path, entry, grading, specs) :
  command = gen_command (submission, path, entry, grading, specs)
  (out, err) = execute_command (command)

  if not err: 
    covers = out.split("Total Exp")[1].split("\n")
    total = int(covers[0].split(" : ")[1])
    cover = int(covers[1].split(" : ")[1])
    uncover = total-cover
    return (total, cover, uncover)
  else:
    return (0, 0, 0)

def exp_cover_module () :
  f=open("cover.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks')

  for (hw,entry,grading,specs) in benchmark_info :
    hw_path = os.path.join (path, hw)

    benchmark_total = 0
    benchmark_cover = 0
    benchmark_uncover = 0

    year_list = find_dir(os.path.join(path,hw))
    for year in year_list :
      submissions = find_files(year)
      for submission in submissions:
        (total, cover, uncover) = check_exp_cover (submission, hw_path, entry, grading, specs)
        print(submission)
        print("Total Exp Number : " + str(total))
        print("Covered Exp Number : " + str(cover))
        print("Uncovered Exp Number : " + str(uncover))
        f.write(submission + "\n")
        f.write("Total Exp Number : " + str(total) + "\n")
        f.write("Covered Exp Number : " + str(cover) + "\n")
        f.write("Uncovered Exp Number : " + str(uncover) + "\n")
        benchmark_total += total
        benchmark_cover += cover
        benchmark_uncover += uncover

    print("Total Exp Number : " + str(benchmark_total))
    print("Covered Exp Number : " + str(benchmark_cover))
    print("Uncovered Exp Number : " + str(benchmark_uncover))
    print("Exp Cover Rate : " + str((benchmark_cover/benchmark_total)*100))
    f.write("===========================================\n")
    f.write("================" + hw + "===============\n")
    f.write("Total Exp Number : " + str(benchmark_total) + "\n")
    f.write("Covered Exp Number : " + str(benchmark_cover) + "\n")
    f.write("Uncovered Exp Number : " + str(benchmark_uncover) + "\n")
    f.write("Exp Cover Rate : " + str((benchmark_cover/benchmark_total)*100) + "\n")
    f.write("===========================================\n")
  
  f.close()

def gen_testcase (submission, path, entry, grading, specs) :
  command = gen_command (submission, path, entry, grading, specs)
  print(command)
  (out, err) = execute_command (command)

  if not err: 
    if (out.find("Correct Code")>=0):
      succ = False
      result = ""
    else :
      succ = True
      result = out.split(".native")[1]
    return (succ, result)
  else:
    return (False, "")

def gen_test_module () :
  f=open("result.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks')

  for (hw,entry,grading,specs) in benchmark_info :
    hw_path = os.path.join (path, hw)

    error_num = 0
    fail_num = 0
    
    year_list = find_dir(os.path.join(path,hw))
    year_list.sort()
    for year in year_list :
      submissions = find_files(year)
      submissions.sort()
      for submission in submissions:
        (succ, result) = gen_testcase (submission, hw_path, entry, grading, specs)
        if(succ):
          print(submission)
          print(result + "\n\n")
          f.write(submission)
          f.write(result + "\n\n")
          error_num += 1
        else:
          print(submission)
          print("Fail to Generate Counter example\n\n")
          #f.write(submission + "\n")
          #f.write("Fail to Generate Counter example\n\n")
          fail_num += 1

    f.write ("===========================================\n")
    f.write ("================" + hw + "===============\n")
    f.write ("Number of Error Programs : " + str(error_num + fail_num) + "\n")
    f.write ("Number of Detected Error programs : " + str(error_num) + "\n")
    f.write ("Number of Failure : " + str(fail_num) + "\n")
    f.write ("===========================================\n")
    f.write ("===========================================\n")
  
  f.close() 

def fix_without_testcase (submission, path, entry, grading, specs) :
  command = gen_command (submission, path, entry, grading, specs)
  print(command)
  (out, err) = execute_command (command)
  if not err: 
    return out
  else:
    return ""

def fix_module () :
  f=open("dd.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks_incorrect')

  for (hw,entry,grading,specs) in benchmark_info :
    hw_path = os.path.join (path, hw)

    error_num = 0
    fail_num = 0
    
    year_list = find_dir(os.path.join(path,hw))
    year_list.sort()
    for year in year_list :
      submissions = find_files(year)
      submissions.sort()
      for submission in submissions:
        out = fix_without_testcase (submission, hw_path, entry, grading, specs)
        if (out.find("None") == -1):
          print(submission + " ")
          print(out)
          f.write(submission + " ")
          f.write(out)
        else:
          print(submission + " ")
          print(out)
          f.write(submission + " ")
          f.write(out)
  f.close()   
  
def fix2_module () :
  f=open("fix2.txt",'w')
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks')

  for (hw,entry,grading,specs) in benchmark_info :
    hw_path = os.path.join (path, hw)

    error_num = 0
    fail_num = 0
    
    year_list = find_dir(os.path.join(path,hw))
    year_list.sort()
    for year in year_list :
      submissions = find_files(year)
      submissions.sort()
      for submission in submissions:
        out = fix_without_testcase (submission, hw_path, entry, grading, specs)
        if (out.find("None") == -1):
          print(submission + " ")
          print(out)
          f.write(submission + " ")
          f.write(out)
        else:
          print(submission + " ")
          print(out)
          f.write(submission + " ")
          f.write(out)
  f.close() 

def main() :
  path = os.path.dirname(__file__)
  path = os.path.join(path, 'benchmarks')

  if (sys.argv[1]=="exp_cover"):
    exp_cover_module ()

  elif (sys.argv[1]=="run"):
    run_module ()

  elif (sys.argv[1]=="gen_test"):
    gen_test_module ()

  elif (sys.argv[1]=="fix"):
    fix_module ()

  elif (sys.argv[1]=="fix2"):
    fix2_module ()

if __name__ == '__main__':
  if len(sys.argv)!=2:
    print("usage : python run.py [mode]")
  else:
    main()
