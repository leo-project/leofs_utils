Usage of disk_status.rb
=======================

Install gems
```
bundle install
```

Start server
```
thin start -C thin.yml
```

Get your disk usage
```
curl http://localhost:7500/dev/sdb1/usage
```

Stop server
```
thin stop -C thin.yml
```
