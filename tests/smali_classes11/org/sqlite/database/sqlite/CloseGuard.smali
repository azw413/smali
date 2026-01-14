.class public final Lorg/sqlite/database/sqlite/CloseGuard;
.super Ljava/lang/Object;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/sqlite/database/sqlite/CloseGuard$DefaultReporter;,
        Lorg/sqlite/database/sqlite/CloseGuard$Reporter;
    }
.end annotation


# static fields
.field private static volatile eZLwKS:Z

.field private static volatile lqDWwP:Lorg/sqlite/database/sqlite/CloseGuard$Reporter;

.field private static final vGuWtK:Lorg/sqlite/database/sqlite/CloseGuard;


# instance fields
.field private mDPCpx:Ljava/lang/Throwable;


# direct methods
.method static constructor <clinit>()V
    .locals 2

    new-instance v0, Lorg/sqlite/database/sqlite/CloseGuard;

    invoke-direct {v0}, Lorg/sqlite/database/sqlite/CloseGuard;-><init>()V

    sput-object v0, Lorg/sqlite/database/sqlite/CloseGuard;->vGuWtK:Lorg/sqlite/database/sqlite/CloseGuard;

    const/4 v0, 0x1

    sput-boolean v0, Lorg/sqlite/database/sqlite/CloseGuard;->eZLwKS:Z

    new-instance v0, Lorg/sqlite/database/sqlite/CloseGuard$DefaultReporter;

    const/4 v1, 0x0

    invoke-direct {v0, v1}, Lorg/sqlite/database/sqlite/CloseGuard$DefaultReporter;-><init>(Lorg/sqlite/database/sqlite/CloseGuard$1;)V

    sput-object v0, Lorg/sqlite/database/sqlite/CloseGuard;->lqDWwP:Lorg/sqlite/database/sqlite/CloseGuard$Reporter;

    return-void
.end method

.method private constructor <init>()V
    .locals 0

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method public static get()Lorg/sqlite/database/sqlite/CloseGuard;
    .locals 1

    sget-boolean v0, Lorg/sqlite/database/sqlite/CloseGuard;->eZLwKS:Z

    if-nez v0, :cond_0

    sget-object v0, Lorg/sqlite/database/sqlite/CloseGuard;->vGuWtK:Lorg/sqlite/database/sqlite/CloseGuard;

    return-object v0

    :cond_0
    new-instance v0, Lorg/sqlite/database/sqlite/CloseGuard;

    invoke-direct {v0}, Lorg/sqlite/database/sqlite/CloseGuard;-><init>()V

    return-object v0
.end method

.method public static getReporter()Lorg/sqlite/database/sqlite/CloseGuard$Reporter;
    .locals 1

    sget-object v0, Lorg/sqlite/database/sqlite/CloseGuard;->lqDWwP:Lorg/sqlite/database/sqlite/CloseGuard$Reporter;

    return-object v0
.end method

.method public static setEnabled(Z)V
    .locals 0

    sput-boolean p0, Lorg/sqlite/database/sqlite/CloseGuard;->eZLwKS:Z

    return-void
.end method

.method public static setReporter(Lorg/sqlite/database/sqlite/CloseGuard$Reporter;)V
    .locals 1

    if-eqz p0, :cond_0

    sput-object p0, Lorg/sqlite/database/sqlite/CloseGuard;->lqDWwP:Lorg/sqlite/database/sqlite/CloseGuard$Reporter;

    return-void

    :cond_0
    new-instance p0, Ljava/lang/NullPointerException;

    const-string v0, "inYF^_K_\u0008\"7\u0000LRJI"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Ljava/lang/NullPointerException;-><init>(Ljava/lang/String;)V

    throw p0
.end method


# virtual methods
.method public close()V
    .locals 1

    const/4 v0, 0x0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/CloseGuard;->mDPCpx:Ljava/lang/Throwable;

    return-void
.end method

.method public open(Ljava/lang/String;)V
    .locals 2

    if-eqz p1, :cond_2

    sget-object v0, Lorg/sqlite/database/sqlite/CloseGuard;->vGuWtK:Lorg/sqlite/database/sqlite/CloseGuard;

    if-eq p0, v0, :cond_1

    sget-boolean v0, Lorg/sqlite/database/sqlite/CloseGuard;->eZLwKS:Z

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "^sYEEHGY\u0008koRONHDDN]G\u001cV[IP@^\u0011\u0013"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, "<+GFX\u000bMLDsoD"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    new-instance v0, Ljava/lang/Throwable;

    invoke-direct {v0, p1}, Ljava/lang/Throwable;-><init>(Ljava/lang/String;)V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/CloseGuard;->mDPCpx:Ljava/lang/Throwable;

    :cond_1
    :goto_0
    return-void

    :cond_2
    new-instance p1, Ljava/lang/NullPointerException;

    const-string v0, "xgFZIY\u000e\u0010\u0015?dUNK"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/NullPointerException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public warnIfOpen()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/CloseGuard;->mDPCpx:Ljava/lang/Throwable;

    if-eqz v0, :cond_1

    sget-boolean v0, Lorg/sqlite/database/sqlite/CloseGuard;->eZLwKS:Z

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    const-string v0, "Z+[L_D[_Kz*WCT\u0006DSVG@N^Z\u001dY[\u001aP@GWVHSD\u0018Y{onc\u001e\\se`c%rrf)r~hxj/htxvwfds-9^NJ\u000eAE_C\tAJ\u0008p@mZX]_R^\u0014_]E\u0018\\XETYgndtAe$fl\'isizhHdx\u0000\u007f{h{l`t}5zaAqx\u001f"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    sget-object v1, Lorg/sqlite/database/sqlite/CloseGuard;->lqDWwP:Lorg/sqlite/database/sqlite/CloseGuard$Reporter;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/CloseGuard;->mDPCpx:Ljava/lang/Throwable;

    invoke-interface {v1, v0, v2}, Lorg/sqlite/database/sqlite/CloseGuard$Reporter;->report(Ljava/lang/String;Ljava/lang/Throwable;)V

    :cond_1
    :goto_0
    return-void
.end method
