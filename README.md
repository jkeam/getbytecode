# GetByteCode
App that gets bytecode for various languages.

## Prerequisites
1. Hugo v0.79.1
2. [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-install.html)

## Development
`hugo server -D`

## Production

### First Time
1. Sign into AWS
```
aws configure
```

2. Create Bucket
```
aws s3api create-bucket --acl public-read --bucket getbytecode --region us-east-1
```

3. Configure S3 to be a website
```
aws s3 website s3://getbytecode/ --index-document index.html
```

4. Create policy file, name it `policy.json`
```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "PublicReadGetObject",
            "Effect": "Allow",
            "Principal": "*",
            "Action": [
                "s3:GetObject"
            ],
            "Resource": [
                "arn:aws:s3:::getbytecode/*"
            ]
        }
    ]
}
```

5. Apply policy
```
aws s3api put-bucket-policy --bucket getbytecode --policy file://policy.json
```

6. Verify
```
curl http://getbytecode.s3-website-us-east-1.amazonaws.com
```

### Build and Deploy
1. Build
```
hugo
```

2. Deploy
```
hugo deploy
```
