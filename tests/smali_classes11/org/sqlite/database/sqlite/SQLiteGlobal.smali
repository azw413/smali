.class public final Lorg/sqlite/database/sqlite/SQLiteGlobal;
.super Ljava/lang/Object;


# static fields
.field private static MciSHT:I = 0x0

.field private static final TdFGAF:Ljava/lang/Object;

.field private static final iFhvYR:Ljava/lang/String; = "SQLiteGlobal"


# direct methods
.method static constructor <clinit>()V
    .locals 1

    new-instance v0, Ljava/lang/Object;

    invoke-direct {v0}, Ljava/lang/Object;-><init>()V

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteGlobal;->TdFGAF:Ljava/lang/Object;

    return-void
.end method

.method private constructor <init>()V
    .locals 0

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method public static getDefaultJournalMode()Ljava/lang/String;
    .locals 1

    const-string v0, "\u007fnELXN"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method public static getDefaultPageSize()I
    .locals 3

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteGlobal;->TdFGAF:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    sget v1, Lorg/sqlite/database/sqlite/SQLiteGlobal;->MciSHT:I

    if-nez v1, :cond_0

    new-instance v1, Landroid/os/StatFs;

    const-string v2, "4oH]M"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v1, v2}, Landroid/os/StatFs;-><init>(Ljava/lang/String;)V

    invoke-virtual {v1}, Landroid/os/StatFs;->getBlockSize()I

    move-result v1

    sput v1, Lorg/sqlite/database/sqlite/SQLiteGlobal;->MciSHT:I

    :cond_0
    monitor-exit v0

    const/16 v0, 0x400

    return v0

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public static getDefaultSyncMode()Ljava/lang/String;
    .locals 1

    const-string v0, "ud[DMG"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method public static getJournalSizeLimit()I
    .locals 1

    const/16 v0, 0x2710

    return v0
.end method

.method public static getWALAutoCheckpoint()I
    .locals 2

    const/16 v0, 0x3e8

    const/4 v1, 0x1

    invoke-static {v1, v0}, Ljava/lang/Math;->max(II)I

    move-result v0

    return v0
.end method

.method public static getWALConnectionPoolSize()I
    .locals 2

    const/16 v0, 0xa

    const/4 v1, 0x2

    invoke-static {v1, v0}, Ljava/lang/Math;->max(II)I

    move-result v0

    return v0
.end method

.method public static getWALSyncMode()Ljava/lang/String;
    .locals 1

    const-string v0, "ud[DMG"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method private static native nativeReleaseMemory()I
.end method

.method public static releaseMemory()I
    .locals 1

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->nativeReleaseMemory()I

    move-result v0

    return v0
.end method
