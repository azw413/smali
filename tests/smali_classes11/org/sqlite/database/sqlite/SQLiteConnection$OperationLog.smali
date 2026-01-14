.class final Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;
.super Ljava/lang/Object;


# annotations
.annotation system Ldalvik/annotation/EnclosingClass;
    value = Lorg/sqlite/database/sqlite/SQLiteConnection;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x1a
    name = "OperationLog"
.end annotation


# static fields
.field static BLVUcw:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static EfxLBS:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static MMVJFr:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static MgdGCp:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static QePWil:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field private static final TCmLFJ:I = 0xff

.field static YhNWCS:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field private static final eCLWuE:I = 0x8

.field static xYIdhi:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field private static final xfcYLB:I = 0x14


# instance fields
.field private BtsAaU:I

.field private final TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

.field private TZkAhs:I


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->NT(Z)V

    return-void
.end method

.method private constructor <init>()V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    const/16 v0, 0x14

    new-array v0, v0, [Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    return-void
.end method

.method synthetic constructor <init>(Lorg/sqlite/database/sqlite/SQLiteConnection$1;)V
    .locals 0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;-><init>()V

    return-void
.end method

.method private EVYNVO(I)I
    .locals 2

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->BtsAaU:I

    add-int/lit8 v1, v0, 0x1

    iput v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->BtsAaU:I

    shl-int/lit8 v0, v0, 0x8

    or-int/2addr p1, v0

    return p1
.end method

.method public static synthetic NT(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->NT(Z)V

    :cond_0
    const-string p0, "F+"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->MMVJFr:Ljava/lang/String;

    const-string p0, "HZe@XNmBFqoCVNIK"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QePWil:Ljava/lang/String;

    const-string p0, ";+dF__\u000e_M|oNVK_\u0005U_WJIO[Y\u0018@JTFRB\\OXS\u0002"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->BLVUcw:Ljava/lang/String;

    const-string p0, "7+"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->xYIdhi:Ljava/lang/String;

    const-string p0, "!+r"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->EfxLBS:Ljava/lang/String;

    const-string p0, ";+\t\t\u0010EACM!"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->MgdGCp:Ljava/lang/String;

    const-string p0, ";+\t\t"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->YhNWCS:Ljava/lang/String;

    return-void
.end method

.method private QshdKl(ILjava/lang/String;)V
    .locals 2

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QstMXH(I)Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    move-result-object p1

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const/4 v1, 0x0

    invoke-virtual {p1, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->describe(Ljava/lang/StringBuilder;Z)V

    if-eqz p2, :cond_0

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->xYIdhi:Ljava/lang/String;

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    :cond_0
    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QePWil:Ljava/lang/String;

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-static {p1, p2}, Landroid/util/Log;->d(Ljava/lang/String;Ljava/lang/String;)I

    return-void
.end method

.method private QstMXH(I)Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;
    .locals 2

    and-int/lit16 v0, p1, 0xff

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    aget-object v0, v1, v0

    iget v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mCookie:I

    if-ne v1, p1, :cond_0

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    return-object v0
.end method

.method private lXvcsv(I)Z
    .locals 3

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QstMXH(I)Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    move-result-object p1

    const/4 v0, 0x0

    if-eqz p1, :cond_0

    invoke-static {}, Landroid/os/SystemClock;->uptimeMillis()J

    move-result-wide v1

    iput-wide v1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mEndTime:J

    const/4 v1, 0x1

    iput-boolean v1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mFinished:Z

    :cond_0
    return v0
.end method


# virtual methods
.method public beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I
    .locals 7

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TZkAhs:I

    add-int/lit8 v1, v1, 0x1

    rem-int/lit8 v1, v1, 0x14

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    aget-object v2, v2, v1

    const/4 v3, 0x0

    const/4 v4, 0x0

    if-nez v2, :cond_0

    new-instance v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    invoke-direct {v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnection$1;)V

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    aput-object v2, v3, v1

    goto :goto_0

    :cond_0
    iput-boolean v4, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mFinished:Z

    iput-object v3, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mException:Ljava/lang/Exception;

    iget-object v3, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    if-eqz v3, :cond_1

    iget-object v3, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    invoke-virtual {v3}, Ljava/util/ArrayList;->clear()V

    :cond_1
    :goto_0
    invoke-static {}, Ljava/lang/System;->currentTimeMillis()J

    move-result-wide v5

    iput-wide v5, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mStartWallTime:J

    invoke-static {}, Landroid/os/SystemClock;->uptimeMillis()J

    move-result-wide v5

    iput-wide v5, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mStartTime:J

    iput-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mKind:Ljava/lang/String;

    iput-object p2, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mSql:Ljava/lang/String;

    if-eqz p3, :cond_4

    iget-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    if-nez p1, :cond_2

    new-instance p1, Ljava/util/ArrayList;

    invoke-direct {p1}, Ljava/util/ArrayList;-><init>()V

    iput-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    goto :goto_1

    :cond_2
    iget-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    invoke-virtual {p1}, Ljava/util/ArrayList;->clear()V

    :goto_1
    array-length p1, p3

    if-ge v4, p1, :cond_4

    aget-object p1, p3, v4

    if-eqz p1, :cond_3

    instance-of p2, p1, [B

    if-eqz p2, :cond_3

    iget-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteConnection;->access$500()[B

    move-result-object p2

    invoke-virtual {p1, p2}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    goto :goto_2

    :cond_3
    iget-object p2, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mBindArgs:Ljava/util/ArrayList;

    invoke-virtual {p2, p1}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    :goto_2
    add-int/lit8 v4, v4, 0x1

    goto :goto_1

    :cond_4
    invoke-direct {p0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->EVYNVO(I)I

    move-result p1

    iput p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mCookie:I

    iput v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TZkAhs:I

    iget p1, v2, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mCookie:I

    monitor-exit v0

    return p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public describeCurrentOperation()Ljava/lang/String;
    .locals 4

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TZkAhs:I

    aget-object v1, v1, v2

    if-eqz v1, :cond_0

    iget-boolean v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mFinished:Z

    if-nez v2, :cond_0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const/4 v3, 0x0

    invoke-virtual {v1, v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->describe(Ljava/lang/StringBuilder;Z)V

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    monitor-exit v0

    return-object v1

    :cond_0
    monitor-exit v0

    const/4 v0, 0x0

    return-object v0

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public dump(Landroid/util/Printer;Z)V
    .locals 7

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->BLVUcw:Ljava/lang/String;

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TZkAhs:I

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    aget-object v2, v2, v1

    if-eqz v2, :cond_2

    const/4 v3, 0x0

    :cond_0
    new-instance v4, Ljava/lang/StringBuilder;

    invoke-direct {v4}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->YhNWCS:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-virtual {v5, v3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v5

    sget-object v6, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->EfxLBS:Ljava/lang/String;

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->access$600(Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->MMVJFr:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v2, v4, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->describe(Ljava/lang/StringBuilder;Z)V

    invoke-virtual {v4}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-interface {p1, v2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    if-lez v1, :cond_1

    add-int/lit8 v1, v1, -0x1

    goto :goto_0

    :cond_1
    const/16 v1, 0x13

    :goto_0
    add-int/lit8 v3, v3, 0x1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    aget-object v2, v2, v1

    if-eqz v2, :cond_3

    const/16 v4, 0x14

    if-lt v3, v4, :cond_0

    goto :goto_1

    :cond_2
    sget-object p2, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->MgdGCp:Ljava/lang/String;

    invoke-interface {p1, p2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_3
    :goto_1
    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public endOperation(I)V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->lXvcsv(I)Z

    move-result v1

    if-eqz v1, :cond_0

    const/4 v1, 0x0

    invoke-direct {p0, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QshdKl(ILjava/lang/String;)V

    :cond_0
    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public endOperationDeferLog(I)Z
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->lXvcsv(I)Z

    move-result p1

    monitor-exit v0

    return p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public failOperation(ILjava/lang/Exception;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QstMXH(I)Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    move-result-object p1

    if-eqz p1, :cond_0

    iput-object p2, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;->mException:Ljava/lang/Exception;

    :cond_0
    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public logOperation(ILjava/lang/String;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->TCsaev:[Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->QshdKl(ILjava/lang/String;)V

    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method
