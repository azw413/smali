.class public Lorg/sqlite/database/sqlite/DatabaseObjectNotClosedException;
.super Ljava/lang/RuntimeException;


# static fields
.field private static final YbWrTu:Ljava/lang/String; = "Application did not close the cursor or database object that was opened here"

.field static mJWAGS:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/DatabaseObjectNotClosedException;->qfC(Z)V

    return-void
.end method

.method public constructor <init>()V
    .locals 1

    sget-object v0, Lorg/sqlite/database/sqlite/DatabaseObjectNotClosedException;->mJWAGS:Ljava/lang/String;

    invoke-direct {p0, v0}, Ljava/lang/RuntimeException;-><init>(Ljava/lang/String;)V

    return-void
.end method

.method public static synthetic qfC(Z)V
    .locals 0
    .annotation system Ldalvik/annotation/MethodParameters;
        accessFlags = {
            0x1000,
            0x1000
        }
        names = {
            "#",
            "#"
        }
    .end annotation

    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Lc;",
            "Lu;",
            "Ln;",
            ")",
            "Lt;"
        }
    .end annotation

    if-eqz p0, :cond_0

    const/4 p0, 0x0

    invoke-static {p0}, Lorg/sqlite/database/sqlite/DatabaseObjectNotClosedException;->qfC(Z)V

    :cond_0
    const-string p0, "Z{YEEHOYApd\u0000FNB\u0005^HF\t_WQN]\u000fNYQ\u0013U@REOJ\n`|-l_\\`fbu`0hpcyxj=lg{e4dwf!xs|cNK\u000eCA[G"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/DatabaseObjectNotClosedException;->mJWAGS:Ljava/lang/String;

    return-void
.end method
