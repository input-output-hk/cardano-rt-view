# Email Notifications

RTView `0.3.0` and higher can send automatic email notifications about specified events (for example, warnings or errors). Click on the bell icon on the top bar to see the corresponding settings.

## SMTP settings

Technically, RTView contains an email client that sends emails using SMTP. That's why you need the SMTP settings of your email provider. Please fill in all inputs marked by an asterisk in `Notifications` -> `How to notify`.

You can use `Test email` button to check if your email settings are correct.

## Note for Gmail users

If you want to set up email notifications using your Gmail account, please check your authentication settings in `Google Account` -> `Security`. And if you use 2FA (most likely you are), you _cannot_ use your regular email password to send notifications from RTView. Instead, go to `Security` -> `App passwords` and generate the new app password (if you don't have one already).

Now you can set up RTView notifications:

1. `SMTP server host`: `smtp.gmail.com`
2. `SMTP server port`: `587`
3. `Username`: most likely, it's your email address
4. `Password`: app password you've generated
5. `SSL`: `STARTTLS`
