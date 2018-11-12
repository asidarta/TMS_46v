
# The namelist that contains some parameter setting is an R file,
# You can then load it by using SOURCE function... isn't it cool?!

s.names <- c("mtt","mll","rhy","vin")
s.group <- c("tms","tms","tms","tms")

if(0){
s.names   <- c( "elv",
                "jia",
                "pyu",
                "pan",
                "nkj",
                "ziw",
                "vic",
                "liz",
                "chr",
                "tub",
                "anj",
                "mel",
                "kay",
                "che",
                "ale",
                #####
                "rac",
                "sar",
                "sus",
                "pho",
                "tom",
                "har",
                "die",
                "nat",
                "sop",
                "ann",
                "osc",
                "leo",
                "emm"#,
                ######
                #"han","tia","jaq","lau"
                )

s.group   <- c( "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                "right",
                ####
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham",
                "sham"#,
                #######
                #"left","left","left","left"
                )
}


motor.log.sep <- " "
nsubj <- length(s.names)


# WM Test setting
nANCHOR   <- 2
nBLOCK.WM <- 6
tasks <- c("Somatic")

# Motor Test setting
nBLOCK.MOTOR <- 5
TRAIN.END <- 4
my.file   <- NULL

