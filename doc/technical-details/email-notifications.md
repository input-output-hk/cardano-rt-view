# Email Notifications

RTView `0.3.0` and higher can send automatic email notifications about specified events (for example, warnings or errors). Click on the bell icon on the top bar to see the corresponding settings.

## SMTP settings

Technically, RTView contains an email client that sends emails using SMTP. That's why you need the SMTP settings of your email provider. Please fill in all inputs marked by an asterisk in `Notifications` -> `How to notify`.

You can use `Test email` button to check if your email settings are correct.

## Note for Gmail users

If you want to set up email notifications using your Gmail account, please make sure that `2-Step Verification` is enabled. You can check it in `Google Account` -> `Security`. After you enabled `2-Step Verification`, please generate the new app password (if you don't have one already) in `Security` -> `App passwords`. You'll need this app password for RTView settings.

Now you can set up RTView notifications:

1. `SMTP server host`: `smtp.gmail.com`
2. `SMTP server port`: `587`
3. `Username`: most likely, it's your email address
4. `Password`: app password you've generated
5. `SSL`: `STARTTLS`

## Check period

By default, the email client in the RTView checks events once per 2 minutes. But this can be configured. To do it, please click on the info icon on the top bar to see `RTView Information`. Here you can find the path to `Notifications file`. This JSON-file contains a field `nsCheckPeriodInSec`, which specifies the check period in seconds. For example, if you set this value to `3600` like this:

```
"nsCheckPeriodInSec": 3600,
```

the email client will check events only once per hour. In this case, you will receive only one email per hour with all events (if any of them occurred).
