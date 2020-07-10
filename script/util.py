import os 
import sys
import subprocess
import shutil

################################################
#       File and Directory manipulation        #
################################################
def find_dir(path):
  whole_list = os.listdir(path)
  result = []
  for name in whole_list :
    full_path = os.path.join(path,name)
    if(os.path.isdir(full_path)):
      result.append(name)
    else:
      continue
  return result

def find_files(path):
  whole_list = os.listdir(path)
  result = []
  for f in whole_list :
    result.append(f)
  return result

def make_dir(directory):
  if not os.path.exists(directory):
    os.makedirs(directory)

def is_exist(path):
  return os.path.exists(path)

def read_file(path):
  f = open(path,'r')
  data = f.read()
  f.close()
  return data

def erase_file(path):
  if os.path.exists(path):
    os.remove(path)
  else:
    pass

def copy_directory(from_path,to_path):
  if os.path.exists(to_path):
    shutil.rmtree(to_path)
  shutil.copytree(from_path,to_path)

def remove_directory(path):
  shutil.rmtree(path)

def compute_loc (path):
  loc = sum(1 for line in open(path))
  return loc

########################################
#       Executing shell command        #
########################################
def execute_command (command):
  p = subprocess.Popen(command,stdout=subprocess.PIPE,stderr=subprocess.PIPE,shell=True)
  out, err = p.communicate ()
  out = out.decode()
  return (out, err)
