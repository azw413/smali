.class public final Lorg/sqlite/database/sqlite/SQLiteCustomFunction;
.super Ljava/lang/Object;


# static fields
.field static liEYCl:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field public final callback:Lorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;

.field public final name:Ljava/lang/String;

.field public final numArgs:I


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->Pua(Z)V

    return-void
.end method

.method public constructor <init>(Ljava/lang/String;ILorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;)V
    .locals 0

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    if-eqz p1, :cond_0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->name:Ljava/lang/String;

    iput p2, p0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->numArgs:I

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->callback:Lorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;

    return-void

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    sget-object p2, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->liEYCl:Ljava/lang/String;

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public static synthetic Pua(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->Pua(Z)V

    :cond_0
    const-string p0, "ujDL\u000cF[^\\?dOV\u0007D@\u0010IGEP\u0015"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->liEYCl:Ljava/lang/String;

    return-void
.end method

.method private dispatchCallback([Ljava/lang/String;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;->callback:Lorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;

    invoke-interface {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;->callback([Ljava/lang/String;)V

    return-void
.end method
