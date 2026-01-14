.class public final Lorg/sqlite/database/sqlite/SQLiteConnectionPool;
.super Ljava/lang/Object;

# interfaces
.implements Ljava/io/Closeable;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;,
        Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;
    }
.end annotation


# static fields
.field public static final CONNECTION_FLAG_INTERACTIVE:I = 0x4

.field public static final CONNECTION_FLAG_PRIMARY_CONNECTION_AFFINITY:I = 0x2

.field public static final CONNECTION_FLAG_READ_ONLY:I = 0x1

.field public static Fvi:[C = null

.field private static final eaRsFY:J = 0x7530L

.field private static final iFhvYR:Ljava/lang/String; = "SQLiteConnectionPool"

.field static final synthetic ltsFhd:Z


# instance fields
.field private CRfRep:I

.field private EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

.field private final HtQxT:Ljava/util/WeakHashMap;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/WeakHashMap<",
            "Lorg/sqlite/database/sqlite/SQLiteConnection;",
            "Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;",
            ">;"
        }
    .end annotation
.end field

.field private final HzNzK:Ljava/util/concurrent/atomic/AtomicBoolean;

.field private final YSONaT:Ljava/util/ArrayList;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteConnection;",
            ">;"
        }
    .end annotation
.end field

.field private final YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

.field private final iAbDrn:Ljava/lang/Object;

.field private maWlUL:Z

.field private mcbtLd:I

.field private vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

.field private xTWzFJ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

.field private final xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x1

    sput-boolean v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->ltsFhd:Z

    return-void
.end method

.method private constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    invoke-static {}, Lorg/sqlite/database/sqlite/CloseGuard;->get()Lorg/sqlite/database/sqlite/CloseGuard;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    new-instance v0, Ljava/lang/Object;

    invoke-direct {v0}, Ljava/lang/Object;-><init>()V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    new-instance v0, Ljava/util/concurrent/atomic/AtomicBoolean;

    invoke-direct {v0}, Ljava/util/concurrent/atomic/AtomicBoolean;-><init>()V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HzNzK:Ljava/util/concurrent/atomic/AtomicBoolean;

    new-instance v0, Ljava/util/ArrayList;

    invoke-direct {v0}, Ljava/util/ArrayList;-><init>()V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    new-instance v0, Ljava/util/WeakHashMap;

    invoke-direct {v0}, Ljava/util/WeakHashMap;-><init>()V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-direct {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BBuhLb()V

    return-void
.end method

.method private BBuhLb()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/high16 v1, 0x20000000

    and-int/2addr v0, v1

    if-eqz v0, :cond_0

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getWALConnectionPoolSize()I

    move-result v0

    goto :goto_0

    :cond_0
    const/4 v0, 0x1

    :goto_0
    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRfRep:I

    return-void
.end method

.method private BbFhcX(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V
    .locals 4

    and-int/lit8 v0, p2, 0x1

    if-eqz v0, :cond_0

    const/4 v0, 0x1

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    :try_start_0
    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->setOnlyAllowReadOnlyOperations(Z)V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->NORMAL:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    invoke-virtual {v0, p1, v1}, Ljava/util/WeakHashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    return-void

    :catch_0
    move-exception v0

    const-string v1, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "]j@EIO\u000eYG?zRGWGWU\u0007SJMNWO]K\u001aR[]XPCBIWD/hbz\u001e[dwpoj~+2jptmtvh:x`)6"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, "7+JFBEKN\\veNdKGBC\u001a"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, p2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-static {v1, p2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    throw v0
.end method

.method private CRjixR(Ljava/lang/Thread;JIZLjava/lang/String;I)Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xTWzFJ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iput-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xTWzFJ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iput-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    goto :goto_0

    :cond_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    invoke-direct {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;)V

    :goto_0
    iput-object p1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mThread:Ljava/lang/Thread;

    iput-wide p2, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mStartTime:J

    iput p4, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mPriority:I

    iput-boolean p5, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mWantPrimaryConnection:Z

    iput-object p6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mSql:Ljava/lang/String;

    iput p7, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mConnectionFlags:I

    return-object v0
.end method

.method private EaUaAw()V
    .locals 2

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    if-eqz v0, :cond_0

    return-void

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/zem\u001eKnjmcfdn}g<kqrt/rpg3tpdy#zaD\\KO\n"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method private QRWNAb(Lorg/sqlite/database/sqlite/SQLiteConnection;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)Z
    .locals 3

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->RECONFIGURE:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    if-ne p2, v0, :cond_0

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception p2

    const-string v0, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "]j@EIO\u000eYG?xEAHHCY@G[Y\u001bLXTJ[BQW\u0016VOXN]I{gbf\u0012\u0008bhlul~`2`h!>"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1, p2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    sget-object p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    :cond_0
    :goto_0
    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    if-ne p2, v0, :cond_1

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    const/4 p1, 0x0

    return p1

    :cond_1
    const/4 p1, 0x1

    return p1
.end method

.method private QxSGZH(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;Z)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 2

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->mcbtLd:I

    add-int/lit8 v1, v0, 0x1

    iput v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->mcbtLd:I

    invoke-static {p0, p1, v0, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->iKXtwT(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;IZ)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object p1

    return-object p1
.end method

.method private static TOYwPT(I)I
    .locals 0

    and-int/lit8 p0, p0, 0x4

    if-eqz p0, :cond_0

    const/4 p0, 0x1

    goto :goto_0

    :cond_0
    const/4 p0, 0x0

    :goto_0
    return p0
.end method

.method private YqseLO(ZI)Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    if-eqz v0, :cond_4

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->TOYwPT(I)I

    move-result p2

    :cond_0
    iget v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mPriority:I

    if-le p2, v1, :cond_1

    goto :goto_1

    :cond_1
    if-nez p1, :cond_3

    iget-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mWantPrimaryConnection:Z

    if-nez v1, :cond_2

    goto :goto_0

    :cond_2
    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    if-nez v0, :cond_0

    goto :goto_1

    :cond_3
    :goto_0
    const/4 p1, 0x1

    return p1

    :cond_4
    :goto_1
    const/4 p1, 0x0

    return p1
.end method

.method static synthetic access$000(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;)Ljava/lang/Object;
    .locals 0

    iget-object p0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    return-object p0
.end method

.method static synthetic access$100(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V
    .locals 0

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vbERsJ(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V

    return-void
.end method

.method private eJfItJ(Ljava/lang/String;I)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 6

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v0

    const/4 v1, 0x0

    const/4 v2, 0x1

    if-le v0, v2, :cond_1

    if-eqz p1, :cond_1

    move v3, v1

    :goto_0
    if-ge v3, v0, :cond_1

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v4, v3}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v4

    check-cast v4, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v4, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->isPreparedStatementInCache(Ljava/lang/String;)Z

    move-result v5

    if-eqz v5, :cond_0

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {p1, v3}, Ljava/util/ArrayList;->remove(I)Ljava/lang/Object;

    invoke-direct {p0, v4, p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BbFhcX(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V

    return-object v4

    :cond_0
    add-int/lit8 v3, v3, 0x1

    goto :goto_0

    :cond_1
    if-lez v0, :cond_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    sub-int/2addr v0, v2

    invoke-virtual {p1, v0}, Ljava/util/ArrayList;->remove(I)Ljava/lang/Object;

    move-result-object p1

    check-cast p1, Lorg/sqlite/database/sqlite/SQLiteConnection;

    :goto_1
    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BbFhcX(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V

    return-object p1

    :cond_2
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {p1}, Ljava/util/WeakHashMap;->size()I

    move-result p1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v0, :cond_3

    add-int/lit8 p1, p1, 0x1

    :cond_3
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRfRep:I

    if-lt p1, v0, :cond_4

    const/4 p1, 0x0

    return-object p1

    :cond_4
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-direct {p0, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QxSGZH(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;Z)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object p1

    goto :goto_1
.end method

.method private eUeiDD(Z)V
    .locals 4

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    if-eqz v0, :cond_1

    if-eqz p1, :cond_0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/CloseGuard;->warnIfOpen()V

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/CloseGuard;->close()V

    :cond_1
    if-nez p1, :cond_3

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter p1

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EaUaAw()V

    const/4 v0, 0x0

    iput-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->larPJk()V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v0}, Ljava/util/WeakHashMap;->size()I

    move-result v0

    if-eqz v0, :cond_2

    const-string v1, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "OcL\tOD@CM|~IMI\u0006U_H^\tZTL\u001d"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v3, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ";cHZ\u000cIKHF?iLMTCA\u0010EG]\u001cOVXJJ\u001aPFV\u0016FT_LT\n"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v2, ";hFGBNMYApdS\u0002NH\u0005ETW\u0007\u001c\u001bjU]V\u001aF]_Z\u0015BS\u0000[F`}hl\u001eIr$wn`i\'s{y;lxtj{bqw6w`th9yD\u000fZCA\tRHGI\u0008"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-static {v1, v0}, Landroid/util/Log;->i(Ljava/lang/String;Ljava/lang/String;)I

    :cond_2
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xjaXeC()V

    monitor-exit p1

    goto :goto_0

    :catchall_0
    move-exception v0

    monitor-exit p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0

    :cond_3
    :goto_0
    return-void
.end method

.method private elTwJK()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v0

    :goto_0
    add-int/lit8 v1, v0, -0x1

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRfRep:I

    add-int/lit8 v2, v2, -0x1

    if-le v0, v2, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0, v1}, Ljava/util/ArrayList;->remove(I)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    move v0, v1

    goto :goto_0

    :cond_0
    return-void
.end method

.method private esNrFN(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)V
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v0}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result v0

    if-nez v0, :cond_2

    new-instance v0, Ljava/util/ArrayList;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->size()I

    move-result v1

    invoke-direct {v0, v1}, Ljava/util/ArrayList;-><init>(I)V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->entrySet()Ljava/util/Set;

    move-result-object v1

    invoke-interface {v1}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v1

    :cond_0
    :goto_0
    invoke-interface {v1}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_1

    invoke-interface {v1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/util/Map$Entry;

    invoke-interface {v2}, Ljava/util/Map$Entry;->getValue()Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    if-eq p1, v3, :cond_0

    sget-object v4, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    if-eq v3, v4, :cond_0

    invoke-interface {v2}, Ljava/util/Map$Entry;->getKey()Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    goto :goto_0

    :cond_1
    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v1

    const/4 v2, 0x0

    :goto_1
    if-ge v2, v1, :cond_2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v0, v2}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v4

    invoke-virtual {v3, v4, p1}, Ljava/util/WeakHashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    add-int/lit8 v2, v2, 0x1

    goto :goto_1

    :cond_2
    return-void
.end method

.method public static iKXtwT(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)Lorg/sqlite/database/sqlite/SQLiteConnectionPool;
    .locals 1

    if-eqz p0, :cond_0

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    invoke-direct {v0, p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    invoke-direct {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iKXtwT()V

    return-object v0

    :cond_0
    new-instance p0, Ljava/lang/IllegalArgumentException;

    const-string v0, "xdGOEL[_IkcOL\u0007KPCS\u0012GSO\u001e_]\u000fTDX_\u0018"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p0
.end method

.method private iKXtwT()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    const/4 v1, 0x1

    invoke-direct {p0, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QxSGZH(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;Z)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iput-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    const-string v1, "xgFZI"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Lorg/sqlite/database/sqlite/CloseGuard;->open(Ljava/lang/String;)V

    return-void
.end method

.method private irdfZY()V
    .locals 8

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string v1, "HZe@XNmBFqoCVNIK`H]E"

    if-eqz v0, :cond_0

    :try_start_0
    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v0, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception v0

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "]j@EIO\u000eYG?xEAHHCY@G[Y\u001b_KYFVPV_S\u0015PDIUK}w-kQFoa`rl\u007fi>)\u007fwqnqa}1}g,5"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-static {v2, v3, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    const/4 v0, 0x0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    :cond_0
    :goto_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v0

    const/4 v2, 0x0

    :goto_1
    if-ge v2, v0, :cond_1

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v3, v2}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Lorg/sqlite/database/sqlite/SQLiteConnection;

    :try_start_1
    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_1

    goto :goto_2

    :catch_1
    move-exception v4

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    new-instance v6, Ljava/lang/StringBuilder;

    invoke-direct {v6}, Ljava/lang/StringBuilder;-><init>()V

    const-string v7, "]j@EIO\u000eYG?xEAHHCY@G[Y\u001b_KYFVPV_S\u0015NYN\u0015Z}g`iLQ!glhkudf`su2={cub}}q5hc99"

    invoke-static {v7}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v6

    invoke-static {v5, v6, v4}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    invoke-direct {p0, v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    add-int/lit8 v4, v2, -0x1

    invoke-virtual {v3, v2}, Ljava/util/ArrayList;->remove(I)Ljava/lang/Object;

    add-int/lit8 v0, v0, -0x1

    move v2, v4

    :goto_2
    add-int/lit8 v2, v2, 0x1

    goto :goto_1

    :cond_1
    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->RECONFIGURE:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->esNrFN(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)V

    return-void
.end method

.method private lHbUVm(Ljava/lang/String;ILandroid/os/CancellationSignal;)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 18

    move-object/from16 v9, p0

    move/from16 v0, p2

    move-object/from16 v10, p3

    and-int/lit8 v1, v0, 0x2

    const/4 v11, 0x1

    const/4 v12, 0x0

    if-eqz v1, :cond_0

    move v6, v11

    goto :goto_0

    :cond_0
    move v6, v12

    :goto_0
    iget-object v13, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v13

    :try_start_0
    invoke-direct/range {p0 .. p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EaUaAw()V

    if-eqz v10, :cond_1

    invoke-virtual/range {p3 .. p3}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_1
    const/4 v14, 0x0

    if-nez v6, :cond_2

    invoke-direct/range {p0 .. p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->eJfItJ(Ljava/lang/String;I)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v1

    goto :goto_1

    :cond_2
    move-object v1, v14

    :goto_1
    if-nez v1, :cond_3

    invoke-direct {v9, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->mxlLfA(I)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v1

    :cond_3
    if-eqz v1, :cond_4

    monitor-exit v13

    return-object v1

    :cond_4
    invoke-static/range {p2 .. p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->TOYwPT(I)I

    move-result v15

    invoke-static {}, Landroid/os/SystemClock;->uptimeMillis()J

    move-result-wide v3

    invoke-static {}, Ljava/lang/Thread;->currentThread()Ljava/lang/Thread;

    move-result-object v2

    move-object/from16 v1, p0

    move v5, v15

    move-object/from16 v7, p1

    move/from16 v8, p2

    invoke-direct/range {v1 .. v8}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRjixR(Ljava/lang/Thread;JIZLjava/lang/String;I)Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    move-result-object v1

    iget-object v2, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    move-object v3, v14

    :goto_2
    if-eqz v2, :cond_6

    iget v4, v2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mPriority:I

    if-le v15, v4, :cond_5

    iput-object v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    goto :goto_3

    :cond_5
    iget-object v3, v2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    move-object/from16 v17, v3

    move-object v3, v2

    move-object/from16 v2, v17

    goto :goto_2

    :cond_6
    :goto_3
    if-eqz v3, :cond_7

    iput-object v1, v3, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    goto :goto_4

    :cond_7
    iput-object v1, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    :goto_4
    iget v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNonce:I

    monitor-exit v13
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_3

    if-eqz v10, :cond_8

    new-instance v3, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;

    invoke-direct {v3, v9, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;I)V

    invoke-virtual {v10, v3}, Landroid/os/CancellationSignal;->setOnCancelListener(Landroid/os/CancellationSignal$OnCancelListener;)V

    :cond_8
    :try_start_1
    iget-wide v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mStartTime:J

    const-wide/16 v4, 0x7530

    add-long/2addr v2, v4

    move-wide v6, v4

    :goto_5
    iget-object v8, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HzNzK:Ljava/util/concurrent/atomic/AtomicBoolean;

    invoke-virtual {v8, v11, v12}, Ljava/util/concurrent/atomic/AtomicBoolean;->compareAndSet(ZZ)Z

    move-result v8

    if-eqz v8, :cond_9

    iget-object v8, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v8
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_2

    :try_start_2
    invoke-direct/range {p0 .. p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xjaXeC()V

    monitor-exit v8

    goto :goto_6

    :catchall_0
    move-exception v0

    monitor-exit v8
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    throw v0

    :cond_9
    :goto_6
    const-wide/32 v15, 0xf4240

    mul-long/2addr v6, v15

    invoke-static {v9, v6, v7}, Ljava/util/concurrent/locks/LockSupport;->parkNanos(Ljava/lang/Object;J)V

    invoke-static {}, Ljava/lang/Thread;->interrupted()Z

    iget-object v6, v9, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v6
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_2

    :try_start_4
    invoke-direct/range {p0 .. p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EaUaAw()V

    iget-object v7, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mAssignedConnection:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iget-object v8, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mException:Ljava/lang/RuntimeException;

    if-nez v7, :cond_c

    if-eqz v8, :cond_a

    goto :goto_8

    :cond_a
    invoke-static {}, Landroid/os/SystemClock;->uptimeMillis()J

    move-result-wide v7

    cmp-long v13, v7, v2

    if-gez v13, :cond_b

    sub-long/2addr v7, v2

    goto :goto_7

    :cond_b
    iget-wide v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mStartTime:J

    sub-long v2, v7, v2

    invoke-direct {v9, v2, v3, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->mxJSSW(JI)V

    add-long/2addr v7, v4

    move-wide v2, v7

    move-wide v7, v4

    :goto_7
    monitor-exit v6

    move-wide v6, v7

    goto :goto_5

    :cond_c
    :goto_8
    invoke-direct {v9, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vpBLHk(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V

    if-eqz v7, :cond_e

    monitor-exit v6
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_1

    if-eqz v10, :cond_d

    invoke-virtual {v10, v14}, Landroid/os/CancellationSignal;->setOnCancelListener(Landroid/os/CancellationSignal$OnCancelListener;)V

    :cond_d
    return-object v7

    :cond_e
    :try_start_5
    throw v8

    :catchall_1
    move-exception v0

    monitor-exit v6
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :try_start_6
    throw v0
    :try_end_6
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception v0

    if-eqz v10, :cond_f

    invoke-virtual {v10, v14}, Landroid/os/CancellationSignal;->setOnCancelListener(Landroid/os/CancellationSignal$OnCancelListener;)V

    :cond_f
    throw v0

    :catchall_3
    move-exception v0

    :try_start_7
    monitor-exit v13
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_3

    throw v0
.end method

.method private lJzEtj()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v0

    const/4 v1, 0x0

    :goto_0
    if-ge v1, v0, :cond_0

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v2, v1}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-direct {p0, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    add-int/lit8 v1, v1, 0x1

    goto :goto_0

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->clear()V

    return-void
.end method

.method private larPJk()V
    .locals 1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lJzEtj()V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v0, :cond_0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    const/4 v0, 0x0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    :cond_0
    return-void
.end method

.method private lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V
    .locals 4

    :try_start_0
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->close()V
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception v0

    const-string v1, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "]j@EIO\u000eYG?iLMTC\u0005SH\\GYXJTWA\u0016\u0011]GE\u0015FWT]\nf}-fQ_!mm&qxb2a}uzn8`|1`{s5lrqzdMZB\u000bcj\u0018\u0007"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-static {v1, p1, v0}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    :goto_0
    return-void
.end method

.method private mxJSSW(JI)V
    .locals 4

    invoke-static {}, Ljava/lang/Thread;->currentThread()Ljava/lang/Thread;

    move-result-object v0

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "OcL\tOD@CM|~IMI\u0006U_H^\tZTL\u001d\\NNPVREP\u0000\u0011"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v3, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const-string v2, "<+AH_\u000bLHMq*ULFDIU\u0007FF\u001c\\L\\V[\u001aP\u0014PY[NSCLC``-|Q\u0008ulqcdt\'"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v0}, Ljava/lang/Thread;->getId()J

    move-result-wide v2

    invoke-virtual {v1, v2, v3}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, ";#"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v0}, Ljava/lang/Thread;->getName()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v2, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v2, "2+"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const-string v0, "lb]A\u000cMBLOl*\u0010Z"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-static {p3}, Ljava/lang/Integer;->toHexString(I)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {v0, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    const-string p3, ";mF[\u000c"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {v1, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p3

    long-to-float p1, p1

    const p2, 0x3a83126f    # 0.001f

    mul-float/2addr p1, p2

    invoke-virtual {p3, p1}, Ljava/lang/StringBuilder;->append(F)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string p2, " seconds.\n"

    invoke-virtual {p1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    new-instance p1, Ljava/util/ArrayList;

    invoke-direct {p1}, Ljava/util/ArrayList;-><init>()V

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {p2}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result p2

    const/4 p3, 0x0

    if-nez p2, :cond_1

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {p2}, Ljava/util/WeakHashMap;->keySet()Ljava/util/Set;

    move-result-object p2

    invoke-interface {p2}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object p2

    move v0, p3

    :goto_0
    invoke-interface {p2}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_2

    invoke-interface {p2}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->describeCurrentOperationUnsafe()Ljava/lang/String;

    move-result-object v2

    if-eqz v2, :cond_0

    invoke-virtual {p1, v2}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    add-int/lit8 p3, p3, 0x1

    goto :goto_0

    :cond_0
    add-int/lit8 v0, v0, 0x1

    goto :goto_0

    :cond_1
    move v0, p3

    :cond_2
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {p2}, Ljava/util/ArrayList;->size()I

    move-result p2

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v2, :cond_3

    add-int/lit8 p2, p2, 0x1

    :cond_3
    const-string v2, "XdGGIHZDGqy\u001a\u0002"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, p3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p3

    const-string v2, ";jJ]E]K\u0001\u0008"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {p3, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v1, v0}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p3

    const-string v0, ";bMEI\u0007\u000e"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p3, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {v1, p2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    const-string p3, " available.\n"

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {p1}, Ljava/util/ArrayList;->isEmpty()Z

    move-result p2

    if-nez p2, :cond_4

    const-string p2, "\nRequests in progress:\n"

    invoke-virtual {v1, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    invoke-virtual {p1}, Ljava/util/ArrayList;->iterator()Ljava/util/Iterator;

    move-result-object p1

    :goto_1
    invoke-interface {p1}, Ljava/util/Iterator;->hasNext()Z

    move-result p2

    if-eqz p2, :cond_4

    invoke-interface {p1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object p2

    check-cast p2, Ljava/lang/String;

    const-string p3, ";+"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {v1, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p3

    invoke-virtual {p3, p2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    const-string p3, "\n"

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    goto :goto_1

    :cond_4
    const-string p1, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-static {p1, p2}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    return-void
.end method

.method private mxlLfA(I)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    :goto_0
    invoke-direct {p0, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BbFhcX(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V

    return-object v0

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v0}, Ljava/util/WeakHashMap;->keySet()Ljava/util/Set;

    move-result-object v0

    invoke-interface {v0}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v0

    :cond_1
    invoke-interface {v0}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_2

    invoke-interface {v0}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->isPrimaryConnection()Z

    move-result v2

    if-eqz v2, :cond_1

    return-object v1

    :cond_2
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    const/4 v1, 0x1

    invoke-direct {p0, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QxSGZH(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;Z)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v0

    goto :goto_0
.end method

.method public static synthetic qEJ(Ljava/lang/String;)Ljava/lang/String;
    .locals 6
    .annotation system Ldalvik/annotation/MethodParameters;
        accessFlags = {
            0xffff,
            0xffff
        }
        names = {
            "{",
            "// Can\'t decompile"
        }
    .end annotation

    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Lf;",
            "Lu;",
            "Lc;",
            ")",
            "Lk;"
        }
    .end annotation

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->Fvi:[C

    const/4 v1, 0x0

    if-nez v0, :cond_0

    const/16 v0, 0x400

    new-array v2, v0, [C

    sput-object v2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->Fvi:[C

    const/4 v2, 0x3

    move v3, v1

    :goto_0
    if-ge v3, v0, :cond_0

    xor-int v4, v2, v3

    add-int/2addr v2, v4

    add-int/lit8 v2, v2, 0x54

    rem-int/lit8 v2, v2, 0x3f

    sget-object v4, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->Fvi:[C

    int-to-char v5, v2

    aput-char v5, v4, v3

    add-int/lit8 v3, v3, 0x1

    goto :goto_0

    :cond_0
    invoke-virtual {p0}, Ljava/lang/String;->length()I

    move-result v0

    new-array v0, v0, [C

    invoke-virtual {p0}, Ljava/lang/String;->toCharArray()[C

    move-result-object v2

    :goto_1
    invoke-virtual {p0}, Ljava/lang/String;->length()I

    move-result v3

    if-ge v1, v3, :cond_1

    aget-char v3, v0, v1

    aget-char v4, v2, v1

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->Fvi:[C

    aget-char v5, v5, v1

    xor-int/2addr v4, v5

    int-to-char v4, v4

    add-int/2addr v3, v4

    int-to-char v3, v3

    aput-char v3, v0, v1

    add-int/lit8 v1, v1, 0x1

    goto :goto_1

    :cond_1
    new-instance p0, Ljava/lang/String;

    invoke-direct {p0, v0}, Ljava/lang/String;-><init>([C)V

    return-object p0
.end method

.method private vKAfGY()V
    .locals 1

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;->DISCARD:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->esNrFN(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)V

    return-void
.end method

.method private vbERsJ(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V
    .locals 3

    iget-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mAssignedConnection:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-nez v0, :cond_5

    iget-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mException:Ljava/lang/RuntimeException;

    if-eqz v0, :cond_0

    goto :goto_3

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    const/4 v1, 0x0

    :goto_0
    if-eq v0, p1, :cond_3

    sget-boolean v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->ltsFhd:Z

    if-nez v1, :cond_2

    if-eqz v0, :cond_1

    goto :goto_1

    :cond_1
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_2
    :goto_1
    iget-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    move-object v2, v1

    move-object v1, v0

    move-object v0, v2

    goto :goto_0

    :cond_3
    iget-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    if-eqz v1, :cond_4

    iput-object v0, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    goto :goto_2

    :cond_4
    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    :goto_2
    new-instance v0, Landroid/os/OperationCanceledException;

    invoke-direct {v0}, Landroid/os/OperationCanceledException;-><init>()V

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mException:Ljava/lang/RuntimeException;

    iget-object p1, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mThread:Ljava/lang/Thread;

    invoke-static {p1}, Ljava/util/concurrent/locks/LockSupport;->unpark(Ljava/lang/Thread;)V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xjaXeC()V

    :cond_5
    :goto_3
    return-void
.end method

.method private vpBLHk(Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xTWzFJ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    const/4 v0, 0x0

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mThread:Ljava/lang/Thread;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mSql:Ljava/lang/String;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mAssignedConnection:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mException:Ljava/lang/RuntimeException;

    iget v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNonce:I

    add-int/lit8 v0, v0, 0x1

    iput v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNonce:I

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xTWzFJ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    return-void
.end method

.method private xjaXeC()V
    .locals 9

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    const/4 v1, 0x0

    const/4 v2, 0x0

    move-object v4, v1

    move v3, v2

    move v5, v3

    :goto_0
    if-eqz v0, :cond_8

    iget-boolean v6, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    const/4 v7, 0x1

    if-nez v6, :cond_0

    goto :goto_2

    :cond_0
    :try_start_0
    iget-boolean v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mWantPrimaryConnection:Z

    if-nez v6, :cond_1

    if-nez v3, :cond_1

    iget-object v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mSql:Ljava/lang/String;

    iget v8, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mConnectionFlags:I

    invoke-direct {p0, v6, v8}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->eJfItJ(Ljava/lang/String;I)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v6

    if-nez v6, :cond_2

    move v3, v7

    goto :goto_1

    :cond_1
    move-object v6, v1

    :cond_2
    :goto_1
    if-nez v6, :cond_3

    if-nez v5, :cond_3

    iget v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mConnectionFlags:I

    invoke-direct {p0, v6}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->mxlLfA(I)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v6

    if-nez v6, :cond_3

    move v5, v7

    :cond_3
    if-eqz v6, :cond_4

    iput-object v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mAssignedConnection:Lorg/sqlite/database/sqlite/SQLiteConnection;
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_2

    :cond_4
    if-eqz v3, :cond_5

    if-eqz v5, :cond_5

    goto :goto_5

    :cond_5
    move v7, v2

    goto :goto_2

    :catch_0
    move-exception v6

    iput-object v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mException:Ljava/lang/RuntimeException;

    :goto_2
    iget-object v6, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    if-eqz v7, :cond_7

    if-eqz v4, :cond_6

    iput-object v6, v4, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    goto :goto_3

    :cond_6
    iput-object v6, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    :goto_3
    iput-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mThread:Ljava/lang/Thread;

    invoke-static {v0}, Ljava/util/concurrent/locks/LockSupport;->unpark(Ljava/lang/Thread;)V

    goto :goto_4

    :cond_7
    move-object v4, v0

    :goto_4
    move-object v0, v6

    goto :goto_0

    :cond_8
    :goto_5
    return-void
.end method


# virtual methods
.method public acquireConnection(Ljava/lang/String;ILandroid/os/CancellationSignal;)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 0

    invoke-direct {p0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lHbUVm(Ljava/lang/String;ILandroid/os/CancellationSignal;)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object p1

    return-object p1
.end method

.method public close()V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->eUeiDD(Z)V

    return-void
.end method

.method public collectDbStats(Ljava/util/ArrayList;)V
    .locals 3
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;",
            ">;)V"
        }
    .end annotation

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v1, :cond_0

    invoke-virtual {v1, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->collectDbStats(Ljava/util/ArrayList;)V

    :cond_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v1}, Ljava/util/ArrayList;->iterator()Ljava/util/Iterator;

    move-result-object v1

    :goto_0
    invoke-interface {v1}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_1

    invoke-interface {v1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v2, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->collectDbStats(Ljava/util/ArrayList;)V

    goto :goto_0

    :cond_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->keySet()Ljava/util/Set;

    move-result-object v1

    invoke-interface {v1}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v1

    :goto_1
    invoke-interface {v1}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_2

    invoke-interface {v1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v2, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->collectDbStatsUnsafe(Ljava/util/ArrayList;)V

    goto :goto_1

    :cond_2
    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public dump(Landroid/util/Printer;Z)V
    .locals 7

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "XdGGIHZDGq*PMHJ\u0005VH@\t"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ":"

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, ";+fYIE\u0014\r"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-boolean v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Z)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, ";+dHT\u000bMBFqoCVNIKC\u001d\u0012"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRfRep:I

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    const-string v1, ";+h_MBBLJso\u0000RUOHQUK\t_TPS]LNX[]\u000c"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v1, :cond_0

    invoke-virtual {v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->dump(Landroid/util/Printer;Z)V

    goto :goto_0

    :cond_0
    const-string v1, "\'eFGI\u0015"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :goto_0
    const-string v1, ";+h_MBBLJso\u0000LHH\u0008@U[D]IG\u001d[@T_QPB\\OXS\u0002"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v1}, Ljava/util/ArrayList;->isEmpty()Z

    move-result v1

    const/4 v2, 0x0

    if-nez v1, :cond_1

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v1}, Ljava/util/ArrayList;->size()I

    move-result v1

    move v3, v2

    :goto_1
    if-ge v3, v1, :cond_2

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v4, v3}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v4

    check-cast v4, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v4, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->dump(Landroid/util/Printer;Z)V

    add-int/lit8 v3, v3, 0x1

    goto :goto_1

    :cond_1
    const-string v1, "\'eFGI\u0015"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_2
    const-string v1, ";+hJ]^G_M{*CMIH@SS[FRH\u0004"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result v1

    if-nez v1, :cond_3

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->entrySet()Ljava/util/Set;

    move-result-object v1

    invoke-interface {v1}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v1

    :goto_2
    invoke-interface {v1}, Ljava/util/Iterator;->hasNext()Z

    move-result v3

    if-eqz v3, :cond_4

    invoke-interface {v1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Ljava/util/Map$Entry;

    invoke-interface {v3}, Ljava/util/Map$Entry;->getKey()Ljava/lang/Object;

    move-result-object v4

    check-cast v4, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v4, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->dumpUnsafe(Landroid/util/Printer;Z)V

    new-instance v4, Ljava/lang/StringBuilder;

    invoke-direct {v4}, Ljava/lang/StringBuilder;-><init>()V

    const-string v5, ";+z]M_[^\u0012?"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-interface {v3}, Ljava/util/Map$Entry;->getValue()Ljava/lang/Object;

    move-result-object v3

    invoke-virtual {v4, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-interface {p1, v3}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    goto :goto_2

    :cond_3
    const-string p2, "\'eFGI\u0015"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-interface {p1, p2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_4
    const-string p2, ";+jFBEKN\\veN\u0002PGLDB@Z\u0006"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-interface {p1, p2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    if-eqz p2, :cond_5

    invoke-static {}, Landroid/os/SystemClock;->uptimeMillis()J

    move-result-wide v3

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vtWKWZ:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    :goto_3
    if-eqz p2, :cond_6

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v5, "!+^HE_KI\u0008yeR\u0002"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-wide v5, p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mStartTime:J

    sub-long v5, v3, v5

    long-to-float v5, v5

    const v6, 0x3a83126f    # 0.001f

    mul-float/2addr v5, v6

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(F)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v5, ";fZ\t\u0001\u000bZEZzkD\u001f"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v5, p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mThread:Ljava/lang/Thread;

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v5, "7+Y[ED\\D\\f7"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget v5, p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mPriority:I

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v5, "7+ZX@\u0016\t"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v5, p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mSql:Ljava/lang/String;

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v5, "\'"

    invoke-virtual {v1, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-interface {p1, v1}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object p2, p2, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNext:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    add-int/lit8 v2, v2, 0x1

    goto :goto_3

    :cond_5
    const-string p2, "\'eFGI\u0015"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-interface {p1, p2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_6
    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public enableLocalizedCollators()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result v1

    if-eqz v1, :cond_0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v1, :cond_0

    invoke-virtual {v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->enableLocalizedCollators()V

    monitor-exit v0

    return-void

    :cond_0
    new-instance v1, Ljava/lang/IllegalStateException;

    const-string v2, "XjGGC_\u000eHF~hLG\u0007JJSF^@F^Z\u001d[@V]UGYGS\u0016WPCck-l_\\`fbu`0na)uu>hkj"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v1, v2}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v1

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method

.method protected finalize()V
    .locals 1

    const/4 v0, 0x1

    :try_start_0
    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->eUeiDD(Z)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    return-void

    :catchall_0
    move-exception v0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    throw v0
.end method

.method onConnectionLeaked()V
    .locals 3

    const-string v0, "HZe@XNmBFqoCVNIK`H]E"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "Z+zx`BZHkpdNGDRL_I\u0012F^Q[^L\u000f\\^F\u0013RTTWBYYj.*"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, "<+^H_\u000bBHItoD\u0003\u0007\u0006u\\BSZY\u001bXT@\u000fC^AA\u0016TPFLQInzdgP\u0008uk#ckt\'f{}um|{{s~z`6|o7skbL]KXW\tRUGUCA@{\t\\RY\u001eO[\u0019Q[WFS\u0003OCo/t|Zjfhqb(rnvb\u0001ck\u0000dm;zv2{w{qaR:eTXu[i\u0006"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HzNzK:Ljava/util/concurrent/atomic/AtomicBoolean;

    const/4 v1, 0x1

    invoke-virtual {v0, v1}, Ljava/util/concurrent/atomic/AtomicBoolean;->set(Z)V

    return-void
.end method

.method public reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    .locals 6

    if-eqz p1, :cond_9

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EaUaAw()V

    iget v1, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    xor-int/2addr v1, v2

    const/high16 v2, 0x20000000

    and-int/2addr v1, v2

    const/4 v2, 0x0

    const/4 v3, 0x1

    if-eqz v1, :cond_0

    move v1, v3

    goto :goto_0

    :cond_0
    move v1, v2

    :goto_0
    if-eqz v1, :cond_3

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v4}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result v4

    if-eqz v4, :cond_2

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lJzEtj()V

    sget-boolean v4, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->ltsFhd:Z

    if-nez v4, :cond_3

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v4}, Ljava/util/ArrayList;->isEmpty()Z

    move-result v4

    if-eqz v4, :cond_1

    goto :goto_1

    :cond_1
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_2
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string v1, "Ly@]I\u000boEM~n\u0000nHABYIU\t\u0014l\u007fq\u0011\u000fW^PV\u0016VAXNW^/lh([F`foca0h`)xrm|zc\u007fu4d~|mr#meN]K\u000bE[G\u0007\\WG]_cJIURPH\u0014P\\\u0017HGYDINy|>=\u000eMmgkt`%g\u007f`\u0001~mAcmzwm{xvf6eN~+CX}[l[G\u0003KED\u0007WV@ZDT\u0010K_I]Y[J]\u0017EjjmgbtWCgw+l`ztb;"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {p1, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_3
    :goto_1
    iget-boolean v4, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    iget-object v5, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-boolean v5, v5, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    if-eq v4, v5, :cond_4

    move v2, v3

    :cond_4
    if-eqz v2, :cond_6

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v2}, Ljava/util/WeakHashMap;->isEmpty()Z

    move-result v2

    if-eqz v2, :cond_5

    goto :goto_2

    :cond_5
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string v1, "]d[LEL@\rczs\u0000aHHVDUS@ROM\u001d[NT_[G\u0016WE\u0016EVKmbhl\u001eGs$govqe~lx;iuqc\u007f1`{sgd7bkh\u000b[\\JJZCD\\LI]_\"@S\u001cMLTSKWDK\u001b\u0016\u0003}Bdfcu\u000ejhe\"szdh`mB~vOcm;uwv7jpzaAin\u0011\\}R-IAWC_M\u0007RT@RPPCJ\u001e^SUT\\[COjjp\"giL_}*"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {p1, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_6
    :goto_2
    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    iget v4, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    if-eq v2, v4, :cond_8

    if-eqz v1, :cond_7

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->larPJk()V

    :cond_7
    invoke-direct {p0, p1, v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QxSGZH(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;Z)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object v1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->larPJk()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->vKAfGY()V

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->updateParametersFrom(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BBuhLb()V

    goto :goto_3

    :cond_8
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v1, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->updateParametersFrom(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->BBuhLb()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->elTwJK()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->irdfZY()V

    :goto_3
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xjaXeC()V

    monitor-exit v0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1

    :cond_9
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string v0, "xdGOEL[_IkcOL\u0007KPCS\u0012GSO\u001e_]\u000fTDX_\u0018"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public releaseConnection(Lorg/sqlite/database/sqlite/SQLiteConnection;)V
    .locals 4

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1, p1}, Ljava/util/WeakHashMap;->remove(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object v1

    check-cast v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;

    if-eqz v1, :cond_6

    iget-boolean v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    if-nez v2, :cond_0

    :goto_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lqzBPK(Lorg/sqlite/database/sqlite/SQLiteConnection;)V

    goto :goto_3

    :cond_0
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->isPrimaryConnection()Z

    move-result v2

    if-eqz v2, :cond_4

    invoke-direct {p0, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QRWNAb(Lorg/sqlite/database/sqlite/SQLiteConnection;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)Z

    move-result v1

    if-eqz v1, :cond_3

    sget-boolean v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->ltsFhd:Z

    if-nez v1, :cond_2

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-nez v1, :cond_1

    goto :goto_1

    :cond_1
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_2
    :goto_1
    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->EeEklj:Lorg/sqlite/database/sqlite/SQLiteConnection;

    :cond_3
    :goto_2
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xjaXeC()V

    goto :goto_3

    :cond_4
    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v2}, Ljava/util/ArrayList;->size()I

    move-result v2

    iget v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->CRfRep:I

    add-int/lit8 v3, v3, -0x1

    if-lt v2, v3, :cond_5

    goto :goto_0

    :cond_5
    invoke-direct {p0, p1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->QRWNAb(Lorg/sqlite/database/sqlite/SQLiteConnection;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$AcquiredConnectionStatus;)Z

    move-result v1

    if-eqz v1, :cond_3

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YSONaT:Ljava/util/ArrayList;

    invoke-virtual {v1, p1}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    goto :goto_2

    :goto_3
    monitor-exit v0

    return-void

    :cond_6
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string v1, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/zem\u001e[qa`ocybv)\u007ftps}lnx{}6b`d#wb_\u000fOHU\\KUMA\u0006U^mD\u001dHUWH\u0014I]XT\u0015YQ\u001bCk|0|Byahf~(gcvb\u0001xzLh\u007fhq}<"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {p1, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public shouldYieldConnection(Lorg/sqlite/database/sqlite/SQLiteConnection;I)Z
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->iAbDrn:Ljava/lang/Object;

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->HtQxT:Ljava/util/WeakHashMap;

    invoke-virtual {v1, p1}, Ljava/util/WeakHashMap;->containsKey(Ljava/lang/Object;)Z

    move-result v1

    if-eqz v1, :cond_1

    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->maWlUL:Z

    if-nez v1, :cond_0

    monitor-exit v0

    const/4 p1, 0x0

    return p1

    :cond_0
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->isPrimaryConnection()Z

    move-result p1

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->YqseLO(ZI)Z

    move-result p1

    monitor-exit v0

    return p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string p2, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/zem\u001e[qa`ocybv)\u007ftps}lnx{}6b`d#wb_\u000fOHU\\KUMA\u0006U^mD\u001dHUWH\u0014I]XT\u0015YQ\u001bCk|0|Byahf~(gcvb\u0001xzLh\u007fhq}<"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1

    :catchall_0
    move-exception p1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method

.method public toString()Ljava/lang/String;
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "HZe@XNmBFqoCVNIK`H]E\u0006\u001b"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method
