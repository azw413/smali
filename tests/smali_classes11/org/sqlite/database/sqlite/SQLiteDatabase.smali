.class public final Lorg/sqlite/database/sqlite/SQLiteDatabase;
.super Lorg/sqlite/database/sqlite/SQLiteClosable;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;,
        Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;
    }
.end annotation


# static fields
.field public static final CONFLICT_ABORT:I = 0x2

.field public static final CONFLICT_FAIL:I = 0x3

.field public static final CONFLICT_IGNORE:I = 0x4

.field public static final CONFLICT_NONE:I = 0x0

.field public static final CONFLICT_REPLACE:I = 0x5

.field public static final CONFLICT_ROLLBACK:I = 0x1

.field public static final CREATE_IF_NECESSARY:I = 0x10000000

.field private static CXLQMA:Ljava/util/WeakHashMap; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/WeakHashMap<",
            "Lorg/sqlite/database/sqlite/SQLiteDatabase;",
            "Ljava/lang/Object;",
            ">;"
        }
    .end annotation
.end field

.field public static final ENABLE_WRITE_AHEAD_LOGGING:I = 0x20000000

.field private static final EcOkpW:I = 0x1

.field public static final MAX_SQL_CACHE_SIZE:I = 0x64

.field public static final NO_LOCALIZED_COLLATORS:I = 0x10

.field public static final OPEN_READONLY:I = 0x1

.field public static final OPEN_READWRITE:I = 0x0

.field public static final SQLITE_MAX_LIKE_PATTERN_LENGTH:I = 0xc350

.field private static final iFhvYR:Ljava/lang/String; = "SQLiteDatabase"

.field private static final iPHiHU:[Ljava/lang/String;

.field static final synthetic ltsFhd:Z

.field private static final miVhQh:I = 0x124fc


# instance fields
.field private final BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

.field private final EmKHgX:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

.field private MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

.field private UnWle:Z

.field private final YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

.field private final iAbDrn:Ljava/lang/Object;

.field private final lMLfeO:Lorg/sqlite/database/sqlite/CloseGuard;

.field private final vAhef:Ljava/lang/ThreadLocal;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/lang/ThreadLocal<",
            "Lorg/sqlite/database/sqlite/SQLiteSession;",
            ">;"
        }
    .end annotation
.end field


# direct methods
.method static constructor <clinit>()V
    .locals 7

    const/4 v0, 0x1

    sput-boolean v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->ltsFhd:Z

    new-instance v0, Ljava/util/WeakHashMap;

    invoke-direct {v0}, Ljava/util/WeakHashMap;-><init>()V

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    const-string v1, ""

    const-string v0, ";D{\t~dbaj^Ik\u0002"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const-string v0, ";D{\tmia\u007f|?"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    const-string v0, ";D{\tjjga\u0008"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    const-string v0, ";D{\tel`bzZ*"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    const-string v0, ";D{\t~n~ai\\O\u0000"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    filled-new-array/range {v1 .. v6}, [Ljava/lang/String;

    move-result-object v0

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iPHiHU:[Ljava/lang/String;

    return-void
.end method

.method private constructor <init>(Ljava/lang/String;ILorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Lorg/sqlite/database/DatabaseErrorHandler;)V
    .locals 1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;-><init>()V

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteDatabase$1;

    invoke-direct {v0, p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase$1;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->vAhef:Ljava/lang/ThreadLocal;

    new-instance v0, Ljava/lang/Object;

    invoke-direct {v0}, Ljava/lang/Object;-><init>()V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    invoke-static {}, Lorg/sqlite/database/sqlite/CloseGuard;->get()Lorg/sqlite/database/sqlite/CloseGuard;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lMLfeO:Lorg/sqlite/database/sqlite/CloseGuard;

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->EmKHgX:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

    if-eqz p4, :cond_0

    goto :goto_0

    :cond_0
    new-instance p4, Lorg/sqlite/database/DefaultDatabaseErrorHandler;

    invoke-direct {p4}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;-><init>()V

    :goto_0
    iput-object p4, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

    new-instance p3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-direct {p3, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;-><init>(Ljava/lang/String;I)V

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    return-void
.end method

.method private Mjtxnf(ZJ)Z
    .locals 2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    const/4 v1, 0x0

    invoke-virtual {v0, p2, p3, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteSession;->yieldTransaction(JZLandroid/os/CancellationSignal;)Z

    move-result p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return p1

    :catchall_0
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method private static YPwLhX()Ljava/util/ArrayList;
    .locals 3
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDatabase;",
            ">;"
        }
    .end annotation

    new-instance v0, Ljava/util/ArrayList;

    invoke-direct {v0}, Ljava/util/ArrayList;-><init>()V

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    monitor-enter v1

    :try_start_0
    sget-object v2, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    invoke-virtual {v2}, Ljava/util/WeakHashMap;->keySet()Ljava/util/Set;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/util/ArrayList;->addAll(Ljava/util/Collection;)Z

    monitor-exit v1

    return-object v0

    :catchall_0
    move-exception v0

    monitor-exit v1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0
.end method

.method private YjTKMU(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;Z)V
    .locals 3

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    if-eqz p2, :cond_0

    const/4 p2, 0x2

    goto :goto_0

    :cond_0
    const/4 p2, 0x1

    :goto_0
    const/4 v1, 0x0

    invoke-virtual {p0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadDefaultConnectionFlags(Z)I

    move-result v1

    const/4 v2, 0x0

    invoke-virtual {v0, p2, p1, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteSession;->beginTransaction(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-void

    :catchall_0
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method private static YsZtBp()Z
    .locals 2

    invoke-static {}, Landroid/os/Looper;->myLooper()Landroid/os/Looper;

    move-result-object v0

    if-eqz v0, :cond_0

    invoke-static {}, Landroid/os/Looper;->getMainLooper()Landroid/os/Looper;

    move-result-object v1

    if-ne v0, v1, :cond_0

    const/4 v0, 0x1

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    return v0
.end method

.method public static create(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 2

    const-string v0, "!fLDCYW\u0017"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/high16 v1, 0x10000000

    invoke-static {v0, p0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;I)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p0

    return-object p0
.end method

.method public static deleteDatabase(Ljava/io/File;)Z
    .locals 5

    if-eqz p0, :cond_1

    invoke-virtual {p0}, Ljava/io/File;->delete()Z

    move-result v0

    const/4 v1, 0x0

    or-int/2addr v0, v1

    new-instance v2, Ljava/io/File;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p0}, Ljava/io/File;->getPath()Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    const-string v4, "6aF\\^EOA"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-direct {v2, v3}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    invoke-virtual {v2}, Ljava/io/File;->delete()Z

    move-result v2

    or-int/2addr v0, v2

    new-instance v2, Ljava/io/File;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p0}, Ljava/io/File;->getPath()Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    const-string v4, "6xAD"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-direct {v2, v3}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    invoke-virtual {v2}, Ljava/io/File;->delete()Z

    move-result v2

    or-int/2addr v0, v2

    new-instance v2, Ljava/io/File;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p0}, Ljava/io/File;->getPath()Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    const-string v4, "6|HE"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-direct {v2, v3}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    invoke-virtual {v2}, Ljava/io/File;->delete()Z

    move-result v2

    or-int/2addr v0, v2

    invoke-virtual {p0}, Ljava/io/File;->getParentFile()Ljava/io/File;

    move-result-object v2

    if-eqz v2, :cond_0

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p0}, Ljava/io/File;->getName()Ljava/lang/String;

    move-result-object p0

    invoke-virtual {v3, p0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p0

    const-string v3, "6fC"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {p0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p0

    invoke-virtual {p0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p0

    new-instance v3, Lorg/sqlite/database/sqlite/SQLiteDatabase$2;

    invoke-direct {v3, p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase$2;-><init>(Ljava/lang/String;)V

    invoke-virtual {v2, v3}, Ljava/io/File;->listFiles(Ljava/io/FileFilter;)[Ljava/io/File;

    move-result-object p0

    if-eqz p0, :cond_0

    array-length v2, p0

    :goto_0
    if-ge v1, v2, :cond_0

    aget-object v3, p0, v1

    invoke-virtual {v3}, Ljava/io/File;->delete()Z

    move-result v3

    or-int/2addr v0, v3

    add-int/lit8 v1, v1, 0x1

    goto :goto_0

    :cond_0
    return v0

    :cond_1
    new-instance p0, Ljava/lang/IllegalArgumentException;

    const-string v0, "}bEL\u000cF[^\\?dOV\u0007D@\u0010IGEP"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p0
.end method

.method static dumpAll(Landroid/util/Printer;Z)V
    .locals 2

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YPwLhX()Ljava/util/ArrayList;

    move-result-object v0

    invoke-virtual {v0}, Ljava/util/ArrayList;->iterator()Ljava/util/Iterator;

    move-result-object v0

    :goto_0
    invoke-interface {v0}, Ljava/util/Iterator;->hasNext()Z

    move-result v1

    if-eqz v1, :cond_0

    invoke-interface {v0}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v1

    check-cast v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-direct {v1, p0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->vKfGAu(Landroid/util/Printer;Z)V

    goto :goto_0

    :cond_0
    return-void
.end method

.method private eUeiDD(Z)V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lMLfeO:Lorg/sqlite/database/sqlite/CloseGuard;

    if-eqz v1, :cond_1

    if-eqz p1, :cond_0

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/CloseGuard;->warnIfOpen()V

    :cond_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lMLfeO:Lorg/sqlite/database/sqlite/CloseGuard;

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/CloseGuard;->close()V

    :cond_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    const/4 v2, 0x0

    iput-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    if-nez p1, :cond_2

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    monitor-enter p1

    :try_start_1
    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    invoke-virtual {v0, p0}, Ljava/util/WeakHashMap;->remove(Ljava/lang/Object;)Ljava/lang/Object;

    monitor-exit p1
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    if-eqz v1, :cond_2

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->close()V

    goto :goto_0

    :catchall_0
    move-exception v0

    :try_start_2
    monitor-exit p1
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw v0

    :cond_2
    :goto_0
    return-void

    :catchall_1
    move-exception p1

    :try_start_3
    monitor-exit v0
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    throw p1
.end method

.method public static findEditTable(Ljava/lang/String;)Ljava/lang/String;
    .locals 3

    invoke-static {p0}, Landroid/text/TextUtils;->isEmpty(Ljava/lang/CharSequence;)Z

    move-result v0

    if-nez v0, :cond_4

    const/16 v0, 0x20

    invoke-virtual {p0, v0}, Ljava/lang/String;->indexOf(I)I

    move-result v0

    const/16 v1, 0x2c

    invoke-virtual {p0, v1}, Ljava/lang/String;->indexOf(I)I

    move-result v1

    const/4 v2, 0x0

    if-lez v0, :cond_1

    if-lt v0, v1, :cond_0

    if-gez v1, :cond_1

    :cond_0
    invoke-virtual {p0, v2, v0}, Ljava/lang/String;->substring(II)Ljava/lang/String;

    move-result-object p0

    return-object p0

    :cond_1
    if-lez v1, :cond_3

    if-lt v1, v0, :cond_2

    if-gez v0, :cond_3

    :cond_2
    invoke-virtual {p0, v2, v1}, Ljava/lang/String;->substring(II)Ljava/lang/String;

    move-result-object p0

    :cond_3
    return-object p0

    :cond_4
    new-instance p0, Ljava/lang/IllegalStateException;

    const-string v0, "Re_H@BJ\r\\~hLGT"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p0
.end method

.method static getDbStats()Ljava/util/ArrayList;
    .locals 3
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;",
            ">;"
        }
    .end annotation

    new-instance v0, Ljava/util/ArrayList;

    invoke-direct {v0}, Ljava/util/ArrayList;-><init>()V

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YPwLhX()Ljava/util/ArrayList;

    move-result-object v1

    invoke-virtual {v1}, Ljava/util/ArrayList;->iterator()Ljava/util/Iterator;

    move-result-object v1

    :goto_0
    invoke-interface {v1}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_0

    invoke-interface {v1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-direct {v2, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->irELdB(Ljava/util/ArrayList;)V

    goto :goto_0

    :cond_0
    return-object v0
.end method

.method public static hasCodec()Z
    .locals 1

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteConnection;->hasCodec()Z

    move-result v0

    return v0
.end method

.method private iKXtwT()V
    .locals 4

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->vwWYBl()V
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_1
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception v0

    goto :goto_1

    :catch_1
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->onCorruption()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->vwWYBl()V
    :try_end_1
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_1 .. :try_end_1} :catch_0

    :goto_0
    return-void

    :goto_1
    const-string v1, "HZe@XNjL\\~hAQB"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "]j@EIO\u000eYG?ePGI\u0006AQSSK]H[\u001d\u001f"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getLabel()Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, "<%"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-static {v1, v2, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V

    throw v0
.end method

.method private irELdB(Ljava/util/ArrayList;)V
    .locals 2
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;",
            ">;)V"
        }
    .end annotation

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-eqz v1, :cond_0

    invoke-virtual {v1, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->collectDbStats(Ljava/util/ArrayList;)V

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

.method private lxEaRU()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-eqz v0, :cond_0

    return-void

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "OcL\tHJZLJ~yE\u0002\u0000"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, "<+@Z\u000cEAY\u0008pzEL\t"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method private mzYtPt()Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/4 v1, 0x1

    and-int/2addr v0, v1

    if-ne v0, v1, :cond_0

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    return v1
.end method

.method public static openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;I)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    const/4 v0, 0x0

    invoke-static {p0, p1, p2, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p0

    return-object p0
.end method

.method public static openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-direct {v0, p0, p2, p1, p3}, Lorg/sqlite/database/sqlite/SQLiteDatabase;-><init>(Ljava/lang/String;ILorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Lorg/sqlite/database/DatabaseErrorHandler;)V

    invoke-direct {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iKXtwT()V

    return-object v0
.end method

.method public static openOrCreateDatabase(Ljava/io/File;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 0

    invoke-virtual {p0}, Ljava/io/File;->getPath()Ljava/lang/String;

    move-result-object p0

    invoke-static {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openOrCreateDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p0

    return-object p0
.end method

.method public static openOrCreateDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 2

    const/high16 v0, 0x10000000

    const/4 v1, 0x0

    invoke-static {p0, p1, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p0

    return-object p0
.end method

.method public static openOrCreateDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Lorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    const/high16 v0, 0x10000000

    invoke-static {p0, p1, v0, p2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p0

    return-object p0
.end method

.method public static releaseMemory()I
    .locals 1

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->releaseMemory()I

    move-result v0

    return v0
.end method

.method private vKfGAu(Landroid/util/Printer;Z)V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-eqz v1, :cond_0

    const-string v1, ""

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    invoke-virtual {v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->dump(Landroid/util/Printer;Z)V

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

.method private vwWYBl()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    sget-boolean v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->ltsFhd:Z

    if-nez v1, :cond_1

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-nez v1, :cond_0

    goto :goto_0

    :cond_0
    new-instance v1, Ljava/lang/AssertionError;

    invoke-direct {v1}, Ljava/lang/AssertionError;-><init>()V

    throw v1

    :cond_1
    :goto_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iKXtwT(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    move-result-object v1

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lMLfeO:Lorg/sqlite/database/sqlite/CloseGuard;

    const-string v2, "xgFZI"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Lorg/sqlite/database/sqlite/CloseGuard;->open(Ljava/lang/String;)V

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    monitor-enter v1

    :try_start_1
    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->CXLQMA:Ljava/util/WeakHashMap;

    const/4 v2, 0x0

    invoke-virtual {v0, p0, v2}, Ljava/util/WeakHashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    monitor-exit v1

    return-void

    :catchall_0
    move-exception v0

    monitor-exit v1
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    throw v0

    :catchall_1
    move-exception v1

    :try_start_2
    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    throw v1
.end method

.method private xhrLXg(Ljava/lang/String;[Ljava/lang/Object;)I
    .locals 2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-static {p1}, Lorg/sqlite/database/DatabaseUtils;->getSqlStatementType(Ljava/lang/String;)I

    move-result v0

    const/4 v1, 0x3

    if-ne v0, v1, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->UnWle:Z

    if-nez v1, :cond_0

    const/4 v1, 0x1

    iput-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->UnWle:Z

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    monitor-exit v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    if-eqz v1, :cond_1

    :try_start_2
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->disableWriteAheadLogging()V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_2

    goto :goto_1

    :catchall_0
    move-exception p1

    :try_start_3
    monitor-exit v0
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_0

    :try_start_4
    throw p1

    :cond_1
    :goto_1
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-direct {v0, p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    :try_start_5
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeUpdateDelete()I

    move-result p1
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :try_start_6
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V
    :try_end_6
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return p1

    :catchall_1
    move-exception p1

    :try_start_7
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :catchall_2
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method


# virtual methods
.method public YjTKMU()V
    .locals 2

    const/4 v0, 0x0

    const/4 v1, 0x1

    invoke-direct {p0, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YjTKMU(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;Z)V

    return-void
.end method

.method public addCustomFunction(Ljava/lang/String;ILorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;)V
    .locals 1

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;

    invoke-direct {v0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;-><init>(Ljava/lang/String;ILorg/sqlite/database/sqlite/SQLiteDatabase$CustomFunction;)V

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter p1

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object p2, p2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {p2, v0}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {p2, p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit p1

    return-void

    :catch_0
    move-exception p2

    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object p3, p3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {p3, v0}, Ljava/util/ArrayList;->remove(Ljava/lang/Object;)Z

    throw p2

    :catchall_0
    move-exception p2

    monitor-exit p1
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw p2
.end method

.method public beginTransactionNonExclusive()V
    .locals 2

    const/4 v0, 0x0

    const/4 v1, 0x0

    invoke-direct {p0, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YjTKMU(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;Z)V

    return-void
.end method

.method public beginTransactionWithListener(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;)V
    .locals 1

    const/4 v0, 0x1

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YjTKMU(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;Z)V

    return-void
.end method

.method public beginTransactionWithListenerNonExclusive(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;)V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YjTKMU(Lorg/sqlite/database/sqlite/SQLiteTransactionListener;Z)V

    return-void
.end method

.method public compileStatement(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteStatement;
    .locals 2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteStatement;

    const/4 v1, 0x0

    invoke-direct {v0, p0, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-object v0

    :catchall_0
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method createSession()Lorg/sqlite/database/sqlite/SQLiteSession;
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteSession;

    invoke-direct {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteSession;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;)V

    return-object v0

    :catchall_0
    move-exception v1

    :try_start_1
    monitor-exit v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    throw v1
.end method

.method public delete(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;)I
    .locals 3

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteStatement;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "_Nelxn\u000ekzPG\u0000"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-static {p2}, Landroid/text/TextUtils;->isEmpty(Ljava/lang/CharSequence;)Z

    move-result v1

    if-nez v1, :cond_0

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, ";\\al~n\u000e"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    goto :goto_0

    :cond_0
    const-string p2, ""

    :goto_0
    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {v0, p0, p1, p3}, Lorg/sqlite/database/sqlite/SQLiteStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    :try_start_1
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeUpdateDelete()I

    move-result p1
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return p1

    :catchall_0
    move-exception p1

    :try_start_3
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    throw p1
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :catchall_1
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method public disableWriteAheadLogging()V
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/high16 v2, 0x20000000

    and-int/2addr v1, v2

    if-nez v1, :cond_0

    monitor-exit v0

    return-void

    :cond_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v3, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const v4, -0x20000001

    and-int/2addr v3, v4

    iput v3, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1, v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return-void

    :catch_0
    move-exception v1

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v4, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    or-int/2addr v2, v4

    iput v2, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    throw v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw v1
.end method

.method public enableLocalizedCollators()V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->enableLocalizedCollators()V

    return-void
.end method

.method public enableWriteAheadLogging()Z
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/high16 v2, 0x20000000

    and-int/2addr v1, v2

    const/4 v3, 0x1

    if-eqz v1, :cond_0

    monitor-exit v0

    return v3

    :cond_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->mzYtPt()Z

    move-result v1

    const/4 v4, 0x0

    if-eqz v1, :cond_1

    monitor-exit v0

    return v4

    :cond_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v1

    if-eqz v1, :cond_2

    const-string v1, "HZe@XNjL\\~hAQB"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    const-string v2, "xjG\u000eX\u000bKCI}fE\u0002pgi\u0010A][\u001cV[PW]C\u0011PRBTBWS]Y!"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-static {v1, v2}, Landroid/util/Log;->i(Ljava/lang/String;Ljava/lang/String;)I

    monitor-exit v0

    return v4

    :cond_2
    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->UnWle:Z

    if-eqz v1, :cond_4

    const-string v1, "HZe@XNjL\\~hAQB"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    const/4 v2, 0x3

    invoke-static {v1, v2}, Landroid/util/Log;->isLoggable(Ljava/lang/String;I)Z

    move-result v1

    if-eqz v1, :cond_3

    const-string v1, "HZe@XNjL\\~hAQB"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "oc@Z\u000cOOYI}kSG\u001d\u0006"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v3, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ";cHZ\u000cJZYI|bEF\u0007BDDFPHO^M\u0013\u0018L[_\u0013G\u0016\u0015EXAZFj.ZIr\u0006"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-static {v1, v2}, Landroid/util/Log;->d(Ljava/lang/String;Ljava/lang/String;)I

    :cond_3
    monitor-exit v0

    return v4

    :cond_4
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v4, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    or-int/2addr v2, v4

    iput v2, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return v3

    :catch_0
    move-exception v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v3, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const v4, -0x20000001

    and-int/2addr v3, v4

    iput v3, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    throw v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw v1
.end method

.method public endTransaction()V
    .locals 2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    const/4 v1, 0x0

    invoke-virtual {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteSession;->endTransaction(Landroid/os/CancellationSignal;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-void

    :catchall_0
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public execSQL(Ljava/lang/String;)V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->xhrLXg(Ljava/lang/String;[Ljava/lang/Object;)I

    return-void
.end method

.method public execSQL(Ljava/lang/String;[Ljava/lang/Object;)V
    .locals 0

    if-eqz p2, :cond_0

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->xhrLXg(Ljava/lang/String;[Ljava/lang/Object;)I

    return-void

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "^fY]U\u000bLDF{KRET"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method protected finalize()V
    .locals 1

    const/4 v0, 0x1

    :try_start_0
    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->eUeiDD(Z)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    return-void

    :catchall_0
    move-exception v0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    throw v0
.end method

.method public getAttachedDbs()Ljava/util/List;
    .locals 5
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/List<",
            "Landroid/util/Pair<",
            "Ljava/lang/String;",
            "Ljava/lang/String;",
            ">;>;"
        }
    .end annotation

    new-instance v0, Ljava/util/ArrayList;

    invoke-direct {v0}, Ljava/util/ArrayList;-><init>()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v1

    :try_start_0
    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    const/4 v3, 0x0

    if-nez v2, :cond_0

    monitor-exit v1

    return-object v3

    :cond_0
    iget-boolean v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->UnWle:Z

    if-nez v2, :cond_1

    new-instance v2, Landroid/util/Pair;

    const-string v3, "vj@G"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v4, v4, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    invoke-direct {v2, v3, v4}, Landroid/util/Pair;-><init>(Ljava/lang/Object;Ljava/lang/Object;)V

    invoke-virtual {v0, v2}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    monitor-exit v1

    return-object v0

    :cond_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    monitor-exit v1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    const-string v1, "kyHNAJ\u000eIIkkBCTCz\\NA]\u0007"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v3}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQuery(Ljava/lang/String;[Ljava/lang/String;)Landroid/database/Cursor;

    move-result-object v3

    :goto_0
    invoke-interface {v3}, Landroid/database/Cursor;->moveToNext()Z

    move-result v1

    if-eqz v1, :cond_2

    new-instance v1, Landroid/util/Pair;

    const/4 v2, 0x1

    invoke-interface {v3, v2}, Landroid/database/Cursor;->getString(I)Ljava/lang/String;

    move-result-object v2

    const/4 v4, 0x2

    invoke-interface {v3, v4}, Landroid/database/Cursor;->getString(I)Ljava/lang/String;

    move-result-object v4

    invoke-direct {v1, v2, v4}, Landroid/util/Pair;-><init>(Ljava/lang/Object;Ljava/lang/Object;)V

    invoke-virtual {v0, v1}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    goto :goto_0

    :cond_2
    if-eqz v3, :cond_3

    :try_start_2
    invoke-interface {v3}, Landroid/database/Cursor;->close()V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    :cond_3
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-object v0

    :catchall_0
    move-exception v0

    if-eqz v3, :cond_4

    :try_start_3
    invoke-interface {v3}, Landroid/database/Cursor;->close()V

    :cond_4
    throw v0
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :catchall_1
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0

    :catchall_2
    move-exception v0

    :try_start_4
    monitor-exit v1
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    throw v0
.end method

.method getLabel()Ljava/lang/String;
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    monitor-exit v0

    return-object v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public getMaximumSize()J
    .locals 4

    const-string v0, "KYhnaj\u000e@IgUPC@CzSHGGH\u0000"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-static {p0, v0, v1}, Lorg/sqlite/database/DatabaseUtils;->longForQuery(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/String;)J

    move-result-wide v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPageSize()J

    move-result-wide v2

    mul-long/2addr v0, v2

    return-wide v0
.end method

.method public getPageSize()J
    .locals 2

    const-string v0, "KYhnaj\u000e]Ixo\u007fQN\\@\u000b"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-static {p0, v0, v1}, Lorg/sqlite/database/DatabaseUtils;->longForQuery(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/String;)J

    move-result-wide v0

    return-wide v0
.end method

.method public final getPath()Ljava/lang/String;
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    monitor-exit v0

    return-object v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public getSyncedTables()Ljava/util/Map;
    .locals 2
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "()",
            "Ljava/util/Map<",
            "Ljava/lang/String;",
            "Ljava/lang/String;",
            ">;"
        }
    .end annotation

    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    new-instance v0, Ljava/util/HashMap;

    const/4 v1, 0x0

    invoke-direct {v0, v1}, Ljava/util/HashMap;-><init>(I)V

    return-object v0
.end method

.method getThreadDefaultConnectionFlags(Z)I
    .locals 1

    if-eqz p1, :cond_0

    const/4 p1, 0x1

    goto :goto_0

    :cond_0
    const/4 p1, 0x2

    :goto_0
    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YsZtBp()Z

    move-result v0

    if-eqz v0, :cond_1

    or-int/lit8 p1, p1, 0x4

    :cond_1
    return p1
.end method

.method getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->vAhef:Ljava/lang/ThreadLocal;

    invoke-virtual {v0}, Ljava/lang/ThreadLocal;->get()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Lorg/sqlite/database/sqlite/SQLiteSession;

    return-object v0
.end method

.method public getVersion()I
    .locals 2

    const-string v0, "KYhnaj\u000eX[zx\u007fTBTVYH\\\u0012"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-static {p0, v0, v1}, Lorg/sqlite/database/DatabaseUtils;->longForQuery(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/String;)J

    move-result-wide v0

    invoke-static {v0, v1}, Ljava/lang/Long;->valueOf(J)Ljava/lang/Long;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/Long;->intValue()I

    move-result v0

    return v0
.end method

.method public inTransaction()Z
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->hasTransaction()Z

    move-result v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return v0

    :catchall_0
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public insert(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;)J
    .locals 2

    const/4 v0, 0x0

    :try_start_0
    invoke-virtual {p0, p1, p2, p3, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->insertWithOnConflict(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;I)J

    move-result-wide p1
    :try_end_0
    .catch Lorg/sqlite/database/SQLException; {:try_start_0 .. :try_end_0} :catch_0

    return-wide p1

    :catch_0
    move-exception p1

    const-string p2, "HZe@XNjL\\~hAQB"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "^y[F^\u000bGC[zxTKIA\u0005"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object p3

    invoke-virtual {p3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p3

    invoke-static {p2, p3, p1}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    const-wide/16 p1, -0x1

    return-wide p1
.end method

.method public insertOrThrow(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;)J
    .locals 1

    const/4 v0, 0x0

    invoke-virtual {p0, p1, p2, p3, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->insertWithOnConflict(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;I)J

    move-result-wide p1

    return-wide p1
.end method

.method public insertWithOnConflict(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;I)J
    .locals 6

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "REzl~\u007f"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iPHiHU:[Ljava/lang/String;

    aget-object p4, v1, p4

    invoke-virtual {v0, p4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const-string p4, ";Bg}c\u000b"

    invoke-static {p4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p4

    invoke-virtual {v0, p4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const/16 p1, 0x28

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;

    const/4 p1, 0x0

    if-eqz p3, :cond_0

    invoke-virtual {p3}, Landroid/content/ContentValues;->size()I

    move-result p4

    if-lez p4, :cond_0

    invoke-virtual {p3}, Landroid/content/ContentValues;->size()I

    move-result p4

    goto :goto_0

    :cond_0
    move p4, p1

    :goto_0
    const/16 v1, 0x29

    if-lez p4, :cond_4

    new-array p2, p4, [Ljava/lang/Object;

    invoke-virtual {p3}, Landroid/content/ContentValues;->keySet()Ljava/util/Set;

    move-result-object v2

    invoke-interface {v2}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v2

    move v3, p1

    :goto_1
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z

    move-result v4

    if-eqz v4, :cond_2

    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v4

    check-cast v4, Ljava/lang/String;

    if-lez v3, :cond_1

    const-string v5, ","

    goto :goto_2

    :cond_1
    const-string v5, ""

    :goto_2
    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    add-int/lit8 v5, v3, 0x1

    invoke-virtual {p3, v4}, Landroid/content/ContentValues;->get(Ljava/lang/String;)Ljava/lang/Object;

    move-result-object v4

    aput-object v4, p2, v3

    move v3, v5

    goto :goto_1

    :cond_2
    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;

    const-string p3, ";]heyn}\r\u0000"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    :goto_3
    if-ge p1, p4, :cond_5

    if-lez p1, :cond_3

    const-string p3, "74"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    goto :goto_4

    :cond_3
    const-string p3, "?"

    :goto_4
    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    add-int/lit8 p1, p1, 0x1

    goto :goto_3

    :cond_4
    new-instance p1, Ljava/lang/StringBuilder;

    invoke-direct {p1}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string p2, "2+\u007fh`~k~\u00087Dunk"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const/4 p2, 0x0

    :cond_5
    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(C)Ljava/lang/StringBuilder;

    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p3

    invoke-direct {p1, p0, p3, p2}, Lorg/sqlite/database/sqlite/SQLiteStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    :try_start_1
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeInsert()J

    move-result-wide p2
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-wide p2

    :catchall_0
    move-exception p2

    :try_start_3
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    throw p2
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :catchall_1
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method public isDatabaseIntegrityOk()Z
    .locals 7

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getAttachedDbs()Ljava/util/List;

    move-result-object v0

    if-eqz v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "\u007fj]HNJ]HDvyT\u0002AIW\n\u0007"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ";hF\\@O@\n\\?hE\u0002UCQBNW_Y_\u0010\u001dH]USUQZL\u0000TE[Kz}h(J@d$ggqqeszy;wn8lv~gvr"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :catchall_0
    move-exception v0

    goto/16 :goto_2

    :catch_0
    move-exception v0

    :try_start_1
    new-instance v0, Ljava/util/ArrayList;

    invoke-direct {v0}, Ljava/util/ArrayList;-><init>()V

    new-instance v1, Landroid/util/Pair;

    const-string v2, "vj@G"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v3

    invoke-direct {v1, v2, v3}, Landroid/util/Pair;-><init>(Ljava/lang/Object;Ljava/lang/Object;)V

    invoke-interface {v0, v1}, Ljava/util/List;->add(Ljava/lang/Object;)Z

    :goto_0
    const/4 v1, 0x0

    move v2, v1

    :goto_1
    invoke-interface {v0}, Ljava/util/List;->size()I

    move-result v3

    if-ge v2, v3, :cond_5

    invoke-interface {v0, v2}, Ljava/util/List;->get(I)Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Landroid/util/Pair;
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    const/4 v4, 0x0

    :try_start_2
    new-instance v5, Ljava/lang/StringBuilder;

    invoke-direct {v5}, Ljava/lang/StringBuilder;-><init>()V

    const-string v6, "KYhnaj\u000e"

    invoke-static {v6}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    iget-object v6, v3, Landroid/util/Pair;->first:Ljava/lang/Object;

    check-cast v6, Ljava/lang/String;

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    const-string v6, "5bG]IL\\D\\fUCJBEN\u0018\u0016\u001b\u0012"

    invoke-static {v6}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-virtual {v5}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v5

    invoke-virtual {p0, v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->compileStatement(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteStatement;

    move-result-object v4

    invoke-virtual {v4}, Lorg/sqlite/database/sqlite/SQLiteStatement;->simpleQueryForString()Ljava/lang/String;

    move-result-object v5

    const-string v6, "t`"

    invoke-static {v6}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v6

    if-nez v6, :cond_2

    const-string v0, "HZe@XNjL\\~hAQB"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v6, "KYhnaj\u000eDFkoGPNR\\oDZL_P\u001eRV\u000f"

    invoke-static {v6}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v2, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, v3, Landroid/util/Pair;->second:Ljava/lang/Object;

    check-cast v3, Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ";yL]YY@HL%*"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-static {v0, v2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    if-eqz v4, :cond_1

    :try_start_3
    invoke-virtual {v4}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_0

    :cond_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return v1

    :cond_2
    if-eqz v4, :cond_3

    :try_start_4
    invoke-virtual {v4}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    :cond_3
    add-int/lit8 v2, v2, 0x1

    goto/16 :goto_1

    :catchall_1
    move-exception v0

    if-eqz v4, :cond_4

    invoke-virtual {v4}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    :cond_4
    throw v0
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_0

    :cond_5
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    const/4 v0, 0x1

    return v0

    :goto_2
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public isDbLockedByCurrentThread()Z
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->hasConnection()Z

    move-result v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return v0

    :catchall_0
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public isDbLockedByOtherThreads()Z
    .locals 1
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    const/4 v0, 0x0

    return v0
.end method

.method public isInMemoryDatabase()Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v1

    monitor-exit v0

    return v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public isOpen()Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-eqz v1, :cond_0

    const/4 v1, 0x1

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    monitor-exit v0

    return v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public isReadOnly()Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->mzYtPt()Z

    move-result v1

    monitor-exit v0

    return v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public isWriteAheadLoggingEnabled()Z
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/high16 v2, 0x20000000

    and-int/2addr v1, v2

    if-eqz v1, :cond_0

    const/4 v1, 0x1

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    monitor-exit v0

    return v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method public markTableSyncable(Ljava/lang/String;Ljava/lang/String;)V
    .locals 0
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    return-void
.end method

.method public markTableSyncable(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V
    .locals 0
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    return-void
.end method

.method public needUpgrade(I)Z
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getVersion()I

    move-result v0

    if-le p1, v0, :cond_0

    const/4 p1, 0x1

    goto :goto_0

    :cond_0
    const/4 p1, 0x0

    :goto_0
    return p1
.end method

.method protected onAllReferencesReleased()V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->eUeiDD(Z)V

    return-void
.end method

.method onCorruption()V
    .locals 2

    const v0, 0x124fc

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getLabel()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1}, Landroid/util/EventLog;->writeEvent(ILjava/lang/String;)I

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

    invoke-interface {v0, p0}, Lorg/sqlite/database/DatabaseErrorHandler;->onCorruption(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    return-void
.end method

.method public query(Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;
    .locals 10

    const/4 v1, 0x0

    const/4 v9, 0x0

    move-object v0, p0

    move-object v2, p1

    move-object v3, p2

    move-object v4, p3

    move-object v5, p4

    move-object v6, p5

    move-object/from16 v7, p6

    move-object/from16 v8, p7

    invoke-virtual/range {v0 .. v9}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->query(ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;

    move-result-object v0

    return-object v0
.end method

.method public query(Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;
    .locals 10

    const/4 v1, 0x0

    move-object v0, p0

    move-object v2, p1

    move-object v3, p2

    move-object v4, p3

    move-object v5, p4

    move-object v6, p5

    move-object/from16 v7, p6

    move-object/from16 v8, p7

    move-object/from16 v9, p8

    invoke-virtual/range {v0 .. v9}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->query(ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;

    move-result-object v0

    return-object v0
.end method

.method public query(ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;
    .locals 12

    const/4 v1, 0x0

    const/4 v11, 0x0

    move-object v0, p0

    move v2, p1

    move-object v3, p2

    move-object v4, p3

    move-object/from16 v5, p4

    move-object/from16 v6, p5

    move-object/from16 v7, p6

    move-object/from16 v8, p7

    move-object/from16 v9, p8

    move-object/from16 v10, p9

    invoke-virtual/range {v0 .. v11}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->queryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object v0

    return-object v0
.end method

.method public query(ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;
    .locals 12

    const/4 v1, 0x0

    move-object v0, p0

    move v2, p1

    move-object v3, p2

    move-object v4, p3

    move-object/from16 v5, p4

    move-object/from16 v6, p5

    move-object/from16 v7, p6

    move-object/from16 v8, p7

    move-object/from16 v9, p8

    move-object/from16 v10, p9

    move-object/from16 v11, p10

    invoke-virtual/range {v0 .. v11}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->queryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object v0

    return-object v0
.end method

.method public queryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;
    .locals 12

    const/4 v11, 0x0

    move-object v0, p0

    move-object v1, p1

    move v2, p2

    move-object v3, p3

    move-object/from16 v4, p4

    move-object/from16 v5, p5

    move-object/from16 v6, p6

    move-object/from16 v7, p7

    move-object/from16 v8, p8

    move-object/from16 v9, p9

    move-object/from16 v10, p10

    invoke-virtual/range {v0 .. v11}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->queryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object v0

    return-object v0
.end method

.method public queryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;
    .locals 9

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    move v1, p2

    move-object v2, p3

    move-object v3, p4

    move-object v4, p5

    move-object/from16 v5, p7

    move-object/from16 v6, p8

    move-object/from16 v7, p9

    move-object/from16 v8, p10

    :try_start_0
    invoke-static/range {v1 .. v8}, Lorg/sqlite/database/sqlite/SQLiteQueryBuilder;->buildQueryString(ZLjava/lang/String;[Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->findEditTable(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    move-object v2, p0

    move-object v3, p1

    move-object v5, p6

    move-object/from16 v7, p11

    invoke-virtual/range {v2 .. v7}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-object v0

    :catchall_0
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public rawQuery(Ljava/lang/String;[Ljava/lang/String;)Landroid/database/Cursor;
    .locals 6

    const/4 v1, 0x0

    const/4 v4, 0x0

    const/4 v5, 0x0

    move-object v0, p0

    move-object v2, p1

    move-object v3, p2

    invoke-virtual/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object p1

    return-object p1
.end method

.method public rawQuery(Ljava/lang/String;[Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;
    .locals 6

    const/4 v1, 0x0

    const/4 v4, 0x0

    move-object v0, p0

    move-object v2, p1

    move-object v3, p2

    move-object v5, p3

    invoke-virtual/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object p1

    return-object p1
.end method

.method public rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;)Landroid/database/Cursor;
    .locals 6

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    move-object v3, p3

    move-object v4, p4

    invoke-virtual/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;

    move-result-object p1

    return-object p1
.end method

.method public rawQueryWithFactory(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)Landroid/database/Cursor;
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;

    invoke-direct {v0, p0, p2, p4, p5}, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)V

    if-eqz p1, :cond_0

    goto :goto_0

    :cond_0
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->EmKHgX:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

    :goto_0
    invoke-interface {v0, p1, p3}, Lorg/sqlite/database/sqlite/SQLiteCursorDriver;->query(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;[Ljava/lang/String;)Landroid/database/Cursor;

    move-result-object p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-object p1

    :catchall_0
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1
.end method

.method public reopenReadWrite()V
    .locals 4

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->mzYtPt()Z

    move-result v1

    if-nez v1, :cond_0

    monitor-exit v0

    return-void

    :cond_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v3, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    and-int/lit8 v3, v3, -0x2

    or-int/lit8 v3, v3, 0x0

    iput v3, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return-void

    :catch_0
    move-exception v2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput v1, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    throw v2

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw v1
.end method

.method public replace(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;)J
    .locals 2

    const/4 v0, 0x5

    :try_start_0
    invoke-virtual {p0, p1, p2, p3, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->insertWithOnConflict(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;I)J

    move-result-wide p1
    :try_end_0
    .catch Lorg/sqlite/database/SQLException; {:try_start_0 .. :try_end_0} :catch_0

    return-wide p1

    :catch_0
    move-exception p1

    const-string p2, "HZe@XNjL\\~hAQB"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "^y[F^\u000bGC[zxTKIA\u0005"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object p3

    invoke-virtual {p3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p3

    invoke-static {p2, p3, p1}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    const-wide/16 p1, -0x1

    return-wide p1
.end method

.method public replaceOrThrow(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;)J
    .locals 1

    const/4 v0, 0x5

    invoke-virtual {p0, p1, p2, p3, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->insertWithOnConflict(Ljava/lang/String;Ljava/lang/String;Landroid/content/ContentValues;I)J

    move-result-wide p1

    return-wide p1
.end method

.method public setForeignKeyConstraintsEnabled(Z)V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-boolean v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    if-ne v1, p1, :cond_0

    monitor-exit v0

    return-void

    :cond_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput-boolean p1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return-void

    :catch_0
    move-exception v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    if-nez p1, :cond_1

    const/4 p1, 0x1

    goto :goto_0

    :cond_1
    const/4 p1, 0x0

    :goto_0
    iput-boolean p1, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    throw v1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw p1
.end method

.method public setLocale(Ljava/util/Locale;)V
    .locals 3

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput-object p1, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {p1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return-void

    :catch_0
    move-exception p1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput-object v1, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;

    throw p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string v0, "wdJH@N\u000e@]l~\u0000LHR\u0005RB\u0012GIWR\u0013"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public setLockingEnabled(Z)V
    .locals 0
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    return-void
.end method

.method public setMaxSqlCacheSize(I)V
    .locals 3

    const/16 v0, 0x64

    if-gt p1, v0, :cond_0

    if-ltz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->lxEaRU()V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->maxSqlCacheSize:I

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput p1, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->maxSqlCacheSize:I
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->MSaCTV:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {p1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    monitor-exit v0

    return-void

    :catch_0
    move-exception p1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDatabase;->BIRswJ:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput v1, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->maxSqlCacheSize:I

    throw p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string v0, "~sYLO_KI\u0008ikLWB\u0006GUSELYU\u001e\r\u0018NTU\u0014\u0002\u0006\u0005"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public setMaximumSize(J)J
    .locals 6

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPageSize()J

    move-result-wide v0

    div-long v2, p1, v0

    rem-long/2addr p1, v0

    const-wide/16 v4, 0x0

    cmp-long p1, p1, v4

    if-eqz p1, :cond_0

    const-wide/16 p1, 0x1

    add-long/2addr v2, p1

    :cond_0
    new-instance p1, Ljava/lang/StringBuilder;

    invoke-direct {p1}, Ljava/lang/StringBuilder;-><init>()V

    const-string p2, "KYhnaj\u000e@IgUPC@CzSHGGH\u001b\u0003\u001d"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1, v2, v3}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    const/4 p2, 0x0

    invoke-static {p0, p1, p2}, Lorg/sqlite/database/DatabaseUtils;->longForQuery(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/String;)J

    move-result-wide p1

    mul-long/2addr p1, v0

    return-wide p1
.end method

.method public setPageSize(J)V
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "KYhnaj\u000e]Ixo\u007fQN\\@\u0010\u001a\u0012"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1, p2}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-virtual {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->execSQL(Ljava/lang/String;)V

    return-void
.end method

.method public setTransactionSuccessful()V
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->setTransactionSuccessful()V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return-void

    :catchall_0
    move-exception v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw v0
.end method

.method public setVersion(I)V
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "KYhnaj\u000eX[zx\u007fTBTVYH\\\t\u0001\u001b"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-virtual {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->execSQL(Ljava/lang/String;)V

    return-void
.end method

.method public toString()Ljava/lang/String;
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "HZe@XNjL\\~hAQB\u001c\u0005"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method public update(Ljava/lang/String;Landroid/content/ContentValues;Ljava/lang/String;[Ljava/lang/String;)I
    .locals 6

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    move-object v3, p3

    move-object v4, p4

    invoke-virtual/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->updateWithOnConflict(Ljava/lang/String;Landroid/content/ContentValues;Ljava/lang/String;[Ljava/lang/String;I)I

    move-result p1

    return p1
.end method

.method public updateWithOnConflict(Ljava/lang/String;Landroid/content/ContentValues;Ljava/lang/String;[Ljava/lang/String;I)I
    .locals 6

    if-eqz p2, :cond_5

    invoke-virtual {p2}, Landroid/content/ContentValues;->size()I

    move-result v0

    if-eqz v0, :cond_5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->acquireReference()V

    :try_start_0
    new-instance v0, Ljava/lang/StringBuilder;

    const/16 v1, 0x78

    invoke-direct {v0, v1}, Ljava/lang/StringBuilder;-><init>(I)V

    const-string v1, "N[mhxn\u000e"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteDatabase;->iPHiHU:[Ljava/lang/String;

    aget-object p5, v1, p5

    invoke-virtual {v0, p5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const-string p1, ";Xl}\u000c"

    invoke-static {p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {p2}, Landroid/content/ContentValues;->size()I

    move-result p1

    if-nez p4, :cond_0

    move p5, p1

    goto :goto_0

    :cond_0
    array-length p5, p4

    add-int/2addr p5, p1

    :goto_0
    new-array v1, p5, [Ljava/lang/Object;

    invoke-virtual {p2}, Landroid/content/ContentValues;->keySet()Ljava/util/Set;

    move-result-object v2

    invoke-interface {v2}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v2

    const/4 v3, 0x0

    :goto_1
    invoke-interface {v2}, Ljava/util/Iterator;->hasNext()Z

    move-result v4

    if-eqz v4, :cond_2

    invoke-interface {v2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v4

    check-cast v4, Ljava/lang/String;

    if-lez v3, :cond_1

    const-string v5, ","

    goto :goto_2

    :cond_1
    const-string v5, ""

    :goto_2
    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    add-int/lit8 v5, v3, 0x1

    invoke-virtual {p2, v4}, Landroid/content/ContentValues;->get(Ljava/lang/String;)Ljava/lang/Object;

    move-result-object v4

    aput-object v4, v1, v3

    const-string v3, "&4"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move v3, v5

    goto :goto_1

    :cond_2
    if-eqz p4, :cond_3

    move p2, p1

    :goto_3
    if-ge p2, p5, :cond_3

    sub-int v2, p2, p1

    aget-object v2, p4, v2

    aput-object v2, v1, p2

    add-int/lit8 p2, p2, 0x1

    goto :goto_3

    :cond_3
    invoke-static {p3}, Landroid/text/TextUtils;->isEmpty(Ljava/lang/CharSequence;)Z

    move-result p1

    if-nez p1, :cond_4

    const-string p1, ";\\al~n\u000e"

    invoke-static {p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    :cond_4
    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteStatement;

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p0, p2, v1}, Lorg/sqlite/database/sqlite/SQLiteStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    :try_start_1
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->executeUpdateDelete()I

    move-result p2
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    return p2

    :catchall_0
    move-exception p2

    :try_start_3
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteStatement;->close()V

    throw p2
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :catchall_1
    move-exception p1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->releaseReference()V

    throw p1

    :cond_5
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "^fY]U\u000bXLDjoS"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public validateSql(Ljava/lang/String;Landroid/os/CancellationSignal;)V
    .locals 3

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    const/4 v1, 0x1

    invoke-virtual {p0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadDefaultConnectionFlags(Z)I

    move-result v1

    const/4 v2, 0x0

    invoke-virtual {v0, p1, v1, p2, v2}, Lorg/sqlite/database/sqlite/SQLiteSession;->prepare(Ljava/lang/String;ILandroid/os/CancellationSignal;Lorg/sqlite/database/sqlite/SQLiteStatementInfo;)V

    return-void
.end method

.method public yieldIfContended()Z
    .locals 3
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    const/4 v0, 0x0

    const-wide/16 v1, -0x1

    invoke-direct {p0, v0, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->Mjtxnf(ZJ)Z

    move-result v0

    return v0
.end method

.method public yieldIfContendedSafely()Z
    .locals 3

    const/4 v0, 0x1

    const-wide/16 v1, -0x1

    invoke-direct {p0, v0, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->Mjtxnf(ZJ)Z

    move-result v0

    return v0
.end method

.method public yieldIfContendedSafely(J)Z
    .locals 1

    const/4 v0, 0x1

    invoke-direct {p0, v0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->Mjtxnf(ZJ)Z

    move-result p1

    return p1
.end method
