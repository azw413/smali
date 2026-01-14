.class public final Lorg/sqlite/database/sqlite/SQLiteConnection;
.super Ljava/lang/Object;

# interfaces
.implements Landroid/os/CancellationSignal$OnCancelListener;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/sqlite/database/sqlite/SQLiteConnection$Operation;,
        Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;,
        Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;,
        Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;
    }
.end annotation


# static fields
.field private static final THOCGY:[Ljava/lang/String;

.field private static final iFhvYR:Ljava/lang/String; = "SQLiteConnection"

.field private static final izjztk:Z = false

.field static final synthetic ltsFhd:Z

.field private static final mkkQfB:[B


# instance fields
.field private final BzDBWC:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

.field private final CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

.field private MECQhq:Z

.field private QIHKJR:J

.field private final TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

.field private YAnwZP:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

.field private final YnHMQY:Z

.field private final YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

.field private final YqccBR:Z

.field private YqxFOI:I

.field private final xijmDw:I

.field private final xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;


# direct methods
.method static constructor <clinit>()V
    .locals 2

    const/4 v0, 0x1

    sput-boolean v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->ltsFhd:Z

    const/4 v0, 0x0

    new-array v1, v0, [Ljava/lang/String;

    sput-object v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->THOCGY:[Ljava/lang/String;

    new-array v0, v0, [B

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->mkkQfB:[B

    return-void
.end method

.method private constructor <init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;IZ)V
    .locals 3

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    invoke-static {}, Lorg/sqlite/database/sqlite/CloseGuard;->get()Lorg/sqlite/database/sqlite/CloseGuard;

    move-result-object v0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    new-instance v1, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const/4 v2, 0x0

    invoke-direct {v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnection$1;)V

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->BzDBWC:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-direct {p1, p2}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iput p3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xijmDw:I

    iput-boolean p4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YnHMQY:Z

    iget p2, p2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/4 p3, 0x1

    and-int/2addr p2, p3

    if-eqz p2, :cond_0

    goto :goto_0

    :cond_0
    const/4 p3, 0x0

    :goto_0
    iput-boolean p3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    new-instance p2, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    iget p1, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->maxSqlCacheSize:I

    invoke-direct {p2, p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    const-string p1, "xgFZI"

    invoke-static {p1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/CloseGuard;->open(Ljava/lang/String;)V

    return-void
.end method

.method private BTEnBb(Landroid/os/CancellationSignal;)V
    .locals 4

    if-eqz p1, :cond_0

    invoke-virtual {p1}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqxFOI:I

    const/4 v1, 0x1

    add-int/2addr v0, v1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqxFOI:I

    if-ne v0, v1, :cond_0

    iget-wide v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v2, v3, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeResetCancel(JZ)V

    invoke-virtual {p1, p0}, Landroid/os/CancellationSignal;->setOnCancelListener(Landroid/os/CancellationSignal$OnCancelListener;)V

    :cond_0
    return-void
.end method

.method private BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V
    .locals 12

    const/4 v0, 0x0

    if-eqz p2, :cond_0

    array-length v1, p2

    goto :goto_0

    :cond_0
    move v1, v0

    :goto_0
    iget v2, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mNumParameters:I

    if-ne v1, v2, :cond_5

    if-nez v1, :cond_1

    return-void

    :cond_1
    iget-wide v10, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    :goto_1
    if-ge v0, v1, :cond_4

    aget-object p1, p2, v0

    invoke-static {p1}, Lorg/sqlite/database/DatabaseUtils;->getTypeOfObject(Ljava/lang/Object;)I

    move-result v2

    packed-switch v2, :pswitch_data_0

    :pswitch_0
    instance-of v2, p1, Ljava/lang/Boolean;

    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    add-int/lit8 v7, v0, 0x1

    if-eqz v2, :cond_3

    check-cast p1, Ljava/lang/Boolean;

    invoke-virtual {p1}, Ljava/lang/Boolean;->booleanValue()Z

    move-result p1

    if-eqz p1, :cond_2

    const-wide/16 v5, 0x1

    goto :goto_3

    :pswitch_1
    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    add-int/lit8 v7, v0, 0x1

    move-object v8, p1

    check-cast v8, [B

    move-wide v5, v10

    invoke-static/range {v3 .. v8}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeBindBlob(JJI[B)V

    goto :goto_4

    :pswitch_2
    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    add-int/lit8 v7, v0, 0x1

    check-cast p1, Ljava/lang/Number;

    invoke-virtual {p1}, Ljava/lang/Number;->doubleValue()D

    move-result-wide v8

    move-wide v5, v10

    invoke-static/range {v3 .. v9}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeBindDouble(JJID)V

    goto :goto_4

    :pswitch_3
    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    add-int/lit8 v7, v0, 0x1

    check-cast p1, Ljava/lang/Number;

    invoke-virtual {p1}, Ljava/lang/Number;->longValue()J

    move-result-wide v8

    :goto_2
    move-wide v5, v10

    invoke-static/range {v3 .. v9}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeBindLong(JJIJ)V

    goto :goto_4

    :pswitch_4
    iget-wide v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    add-int/lit8 p1, v0, 0x1

    invoke-static {v2, v3, v10, v11, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeBindNull(JJI)V

    goto :goto_4

    :cond_2
    const-wide/16 v5, 0x0

    :goto_3
    move-wide v8, v5

    goto :goto_2

    :cond_3
    invoke-virtual {p1}, Ljava/lang/Object;->toString()Ljava/lang/String;

    move-result-object v8

    move-wide v5, v10

    invoke-static/range {v3 .. v8}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeBindString(JJILjava/lang/String;)V

    :goto_4
    add-int/lit8 v0, v0, 0x1

    goto :goto_1

    :cond_4
    return-void

    :cond_5
    new-instance p2, Lorg/sqlite/database/sqlite/SQLiteBindOrColumnIndexOutOfRangeException;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "^sYLO_KI\u0008"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget p1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mNumParameters:I

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";i@GH\u000bO_OjgELSU\u0005RRF\t"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";|L[I\u000b^_GicDGC\u0008"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p2, p1}, Lorg/sqlite/database/sqlite/SQLiteBindOrColumnIndexOutOfRangeException;-><init>(Ljava/lang/String;)V

    throw p2

    :pswitch_data_0
    .packed-switch 0x0
        :pswitch_4
        :pswitch_3
        :pswitch_2
        :pswitch_0
        :pswitch_1
    .end packed-switch
.end method

.method private BkUSlK()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v0

    if-nez v0, :cond_1

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-nez v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    const/high16 v1, 0x20000000

    and-int/2addr v0, v1

    if-eqz v0, :cond_0

    const-string v0, "LJe"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->emPaMN(Ljava/lang/String;)V

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getWALSyncMode()Ljava/lang/String;

    move-result-object v0

    goto :goto_0

    :cond_0
    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getDefaultJournalMode()Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->emPaMN(Ljava/lang/String;)V

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getDefaultSyncMode()Ljava/lang/String;

    move-result-object v0

    :goto_0
    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->CvdndN(Ljava/lang/String;)V

    :cond_1
    return-void
.end method

.method private CDbudh()V
    .locals 6

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v0

    if-nez v0, :cond_0

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-nez v0, :cond_0

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getDefaultPageSize()I

    move-result v0

    int-to-long v0, v0

    const-string v2, "KYhnaj\u000e]Ixo\u007fQN\\@"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x0

    invoke-virtual {p0, v2, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v4

    cmp-long v2, v4, v0

    if-eqz v2, :cond_0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "KYhnaj\u000e]Ixo\u007fQN\\@\r"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0, v1}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p0, v0, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    :cond_0
    return-void
.end method

.method private static CHDGKD(Ljava/lang/String;)Ljava/lang/String;
    .locals 1

    const-string v0, "0"

    invoke-virtual {p0, v0}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_0

    const-string p0, "TMo"

    :goto_0
    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    return-object p0

    :cond_0
    const-string v0, "1"

    invoke-virtual {p0, v0}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_1

    const-string p0, "UD{dmg"

    goto :goto_0

    :cond_1
    const-string v0, "2"

    invoke-virtual {p0, v0}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v0

    if-eqz v0, :cond_2

    const-string p0, "]^ee"

    goto :goto_0

    :cond_2
    return-object p0
.end method

.method private CvdndN(Ljava/lang/String;)V
    .locals 3

    const-string v0, "KYhnaj\u000e^QqiHPHHJET"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-virtual {p0, v0, v1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object v0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->CHDGKD(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-static {p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->CHDGKD(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v0

    if-nez v0, :cond_0

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "KYhnaj\u000e^QqiHPHHJET\u000f"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-virtual {p0, p1, v1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    :cond_0
    return-void
.end method

.method private MPPfhV()V
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    and-int/lit8 v0, v0, 0x10

    if-eqz v0, :cond_0

    return-void

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;

    invoke-virtual {v0}, Ljava/util/Locale;->toString()Ljava/lang/String;

    move-result-object v0

    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v1, v2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeRegisterLocalizedCollators(JLjava/lang/String;)V

    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-eqz v1, :cond_1

    return-void

    :cond_1
    :try_start_0
    const-string v1, "XYlhxn\u000eyi]Fe\u0002n`\u0005~hf\tycwnl|\u001aPZWDZIR\u007fUO{oiiJI!,oifqkw)H^FI1"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    const/4 v2, 0x0

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    const-string v1, "HNelo\u007f\u000eAG|kLG\u0007`w\u007fj\u0012HR_LRQKe\\QGWQABA\u0018\u007fAGBF\u001e{DHFEQ0IGEP;QO\\JH1VJ6yntbuh\u000bkkxg\tnnelr\u0013\u001d"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object v1

    if-eqz v1, :cond_2

    invoke-virtual {v1, v0}, Ljava/lang/String;->equals(Ljava/lang/Object;)Z

    move-result v1

    if-eqz v1, :cond_2

    return-void

    :cond_2
    const-string v1, "YNn`b"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    :try_start_1
    const-string v1, "_Nelxn\u000ekzPG\u0000CIBW_NVvQ^J\\\\NNP"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    const-string v1, "REzl~\u007f\u000edfKE\u0000CIBW_NVvQ^J\\\\NNP\u0014\u001bZZCWL]\u0003/XLDkmR,</"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    filled-new-array {v0}, [Ljava/lang/Object;

    move-result-object v3

    invoke-virtual {p0, v1, v3, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    const-string v1, "IN`ghnv\rdPIann|`t"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :try_start_2
    const-string v1, "XDdde\u007f"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0, v1, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    return-void

    :catchall_0
    move-exception v1

    const-string v3, "IDeenjmf"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {p0, v3, v2, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    throw v1
    :try_end_2
    .catch Ljava/lang/RuntimeException; {:try_start_2 .. :try_end_2} :catch_0

    :catch_0
    move-exception v1

    new-instance v2, Lorg/sqlite/database/sqlite/SQLiteException;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "]j@EIO\u000eYG?iHCIA@\u0010K]J]W[\u001d^@H\u0011PQ\u0016\u0012"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v4, v4, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    const-string v4, "<+]F\u000c\u000c"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v3, "<%"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-direct {v2, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteException;-><init>(Ljava/lang/String;Ljava/lang/Throwable;)V

    throw v2
.end method

.method private QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 4

    const/4 v0, 0x0

    iput-boolean v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInUse:Z

    iget-boolean v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z

    if-eqz v0, :cond_0

    :try_start_0
    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v2, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v0, v1, v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeResetStatementAndClearBindings(JJ)V
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception v0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    iget-object p1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mSql:Ljava/lang/String;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->remove(Ljava/lang/Object;)Ljava/lang/Object;

    goto :goto_0

    :cond_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xUYWJB(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    :goto_0
    return-void
.end method

.method private QsZxsD()V
    .locals 6

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v0

    if-nez v0, :cond_0

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-nez v0, :cond_0

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getWALAutoCheckpoint()I

    move-result v0

    int-to-long v0, v0

    const-string v2, "KYhnaj\u000eZIsUAWSIFXBQBLTWSL"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x0

    invoke-virtual {p0, v2, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v4

    cmp-long v2, v4, v0

    if-eqz v2, :cond_0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "KYhnaj\u000eZIsUAWSIFXBQBLTWSL\u0012"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0, v1}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p0, v0, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    :cond_0
    return-void
.end method

.method private TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 1

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->MECQhq:Z

    if-eqz v0, :cond_1

    iget-boolean p1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mReadOnly:Z

    if-eqz p1, :cond_0

    goto :goto_0

    :cond_0
    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteException;

    const-string v0, "XjGGC_\u000eHPziUVB\u0006QXNA\tOO_I]B__@\u0013TPCWUKO/gy(SAflw&h\u007fc{oe;ju}/~p`rttrr#{x_\u000fZCA\tAHFKCPXkFS\u001cTM\u001bF\\SS\u0015ZXOB\u0005"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_1
    :goto_0
    return-void
.end method

.method private static ThtFxb(Ljava/lang/String;)Ljava/lang/String;
    .locals 2

    const-string v0, "@WZt\u0006w@\u0006sCy}\u0008"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const-string v1, " "

    invoke-virtual {p0, v0, v1}, Ljava/lang/String;->replaceAll(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    return-object p0
.end method

.method private YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 0

    return-void
.end method

.method private YsHhQj()V
    .locals 6

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->isInMemoryDb()Z

    move-result v0

    if-nez v0, :cond_0

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-nez v0, :cond_0

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteGlobal;->getJournalSizeLimit()I

    move-result v0

    int-to-long v0, v0

    const-string v2, "KYhnaj\u000eGGjxNCKyVY]WvPRSTL"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x0

    invoke-virtual {p0, v2, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v4

    cmp-long v2, v4, v0

    if-eqz v2, :cond_0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "KYhnaj\u000eGGjxNCKyVY]WvPRSTL\u0012"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0, v1}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p0, v0, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    :cond_0
    return-void
.end method

.method static synthetic access$200(Lorg/sqlite/database/sqlite/SQLiteConnection;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 0

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xUYWJB(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    return-void
.end method

.method static synthetic access$300(Ljava/lang/String;)Ljava/lang/String;
    .locals 0

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->ThtFxb(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    return-object p0
.end method

.method static synthetic access$500()[B
    .locals 1

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->mkkQfB:[B

    return-object v0
.end method

.method private eUeiDD(Z)V
    .locals 4

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    if-eqz v0, :cond_1

    if-eqz p1, :cond_0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/CloseGuard;->warnIfOpen()V

    :cond_0
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqTZwc:Lorg/sqlite/database/sqlite/CloseGuard;

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/CloseGuard;->close()V

    :cond_1
    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    const-wide/16 v2, 0x0

    cmp-long p1, v0, v2

    if-eqz p1, :cond_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v0, "xgFZI"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-virtual {p1, v0, v1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result p1

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->evictAll()V

    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeClose(J)V

    iput-wide v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    goto :goto_0

    :catchall_0
    move-exception v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v1, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw v0

    :cond_2
    :goto_0
    return-void
.end method

.method private eZKvwk(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 1

    const/4 v0, 0x0

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mSql:Ljava/lang/String;

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YAnwZP:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mPoolNext:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YAnwZP:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    return-void
.end method

.method private emPaMN(Ljava/lang/String;)V
    .locals 4

    const-string v0, "KYhnaj\u000eGGjxNCKyH_CW"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    const/4 v1, 0x0

    invoke-virtual {p0, v0, v1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v2

    if-nez v2, :cond_1

    :try_start_0
    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "KYhnaj\u000eGGjxNCKyH_CW\u0014"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-virtual {p0, v2, v1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v1
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseLockedException; {:try_start_0 .. :try_end_0} :catch_0

    if-eqz v1, :cond_0

    return-void

    :catch_0
    move-exception v1

    :cond_0
    const-string v1, "HZe@XNmBFqoCVNIK"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v3, "Xd\\EH\u000b@B\\?iHCIA@\u0010SZL\u001c__IYM[BQ\u0013\\ZUDNYF/cbl[\u0008nb#!"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v3, v3, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, "<+O[CF\u000e\n"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v2, "<+]F\u000c\u000c"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v0, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, "<+KLOJ[^M?~HG\u0007BDDFPHO^\u001eTK\u000fV^WXSQ\u000e\u0016\u0000lBf}-}M]`ho\u007f%}bsgo;juy{:e|vdp!vq|-D[FNV\tMWMK\u0006PClGX_IWTZJ\u0012CW\u0015BK^\u000bnnd|Ljwl\"p`le{,QxzVhpog9f\u007f}5reT{iPNt\u001ekZMN\nLFFTY]]U\u0011_]\u001eYUH[[T^Hb$tpht[\u0001hlnkm(kyrsz|v0bqyy5:9HehF@Ayg@W\"RDXMIR\\\u0019YS]SYiAg\u0011ATP\u0016]W\\xema.b\u007fEg-"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-static {v1, p1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    :cond_1
    return-void
.end method

.method public static hasCodec()Z
    .locals 1

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeHasCodec()Z

    move-result v0

    return v0
.end method

.method private static iFJWxs(I)Z
    .locals 2

    const/4 v0, 0x2

    const/4 v1, 0x1

    if-eq p0, v0, :cond_1

    if-ne p0, v1, :cond_0

    goto :goto_0

    :cond_0
    const/4 p0, 0x0

    return p0

    :cond_1
    :goto_0
    return v1
.end method

.method static iKXtwT(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;IZ)Lorg/sqlite/database/sqlite/SQLiteConnection;
    .locals 1

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-direct {v0, p0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;IZ)V

    :try_start_0
    invoke-direct {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->iKXtwT()V
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_0

    return-object v0

    :catch_0
    move-exception p0

    const/4 p1, 0x0

    invoke-direct {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->eUeiDD(Z)V

    throw p0
.end method

.method private iKXtwT()V
    .locals 5

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->label:Ljava/lang/String;

    sget-boolean v3, Lorg/sqlite/database/sqlite/SQLiteDebug;->DEBUG_SQL_STATEMENTS:Z

    sget-boolean v4, Lorg/sqlite/database/sqlite/SQLiteDebug;->DEBUG_SQL_TIME:Z

    invoke-static {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeOpen(Ljava/lang/String;ILjava/lang/String;ZZ)J

    move-result-wide v0

    iput-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->CDbudh()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->vVTkjP()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YsHhQj()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QsZxsD()V

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeHasCodec()Z

    move-result v0

    if-nez v0, :cond_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BkUSlK()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->MPPfhV()V

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {v0}, Ljava/util/ArrayList;->size()I

    move-result v0

    const/4 v1, 0x0

    :goto_0
    if-ge v1, v0, :cond_1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {v2, v1}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;

    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v3, v4, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeRegisterCustomFunction(JLorg/sqlite/database/sqlite/SQLiteCustomFunction;)V

    add-int/lit8 v1, v1, 0x1

    goto :goto_0

    :cond_1
    return-void
.end method

.method private iVVBrW(IJJ)Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;
    .locals 12

    move-object v0, p0

    iget-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    iget-boolean v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YnHMQY:Z

    if-nez v2, :cond_0

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v2, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ";#"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xijmDw:I

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ")"

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    :cond_0
    move-object v3, v1

    new-instance v1, Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->hitCount()I

    move-result v9

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->missCount()I

    move-result v10

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->size()I

    move-result v11

    move-object v2, v1

    move-wide v4, p2

    move-wide/from16 v6, p4

    move v8, p1

    invoke-direct/range {v2 .. v11}, Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;-><init>(Ljava/lang/String;JJIIII)V

    return-object v1
.end method

.method private izMVIa(Ljava/lang/String;JIIZ)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YAnwZP:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mPoolNext:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    iput-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YAnwZP:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    iput-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mPoolNext:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    const/4 v1, 0x0

    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z

    goto :goto_0

    :cond_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    invoke-direct {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;-><init>(Lorg/sqlite/database/sqlite/SQLiteConnection$1;)V

    :goto_0
    iput-object p1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mSql:Ljava/lang/String;

    iput-wide p2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    iput p4, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mNumParameters:I

    iput p5, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mType:I

    iput-boolean p6, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mReadOnly:Z

    return-object v0
.end method

.method private mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;
    .locals 13

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    const/4 v1, 0x1

    if-eqz v0, :cond_1

    iget-boolean v2, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInUse:Z

    if-nez v2, :cond_0

    return-object v0

    :cond_0
    move v2, v1

    goto :goto_0

    :cond_1
    const/4 v2, 0x0

    :goto_0
    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v3, v4, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativePrepareStatement(JLjava/lang/String;)J

    move-result-wide v3

    :try_start_0
    iget-wide v5, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v5, v6, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeGetParameterCount(JJ)I

    move-result v9

    invoke-static {p1}, Lorg/sqlite/database/DatabaseUtils;->getSqlStatementType(Ljava/lang/String;)I

    move-result v12

    iget-wide v5, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v5, v6, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeIsReadOnly(JJ)Z

    move-result v11

    move-object v5, p0

    move-object v6, p1

    move-wide v7, v3

    move v10, v12

    invoke-direct/range {v5 .. v11}, Lorg/sqlite/database/sqlite/SQLiteConnection;->izMVIa(Ljava/lang/String;JIIZ)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object v0

    if-nez v2, :cond_2

    invoke-static {v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->iFJWxs(I)Z

    move-result v2

    if-eqz v2, :cond_2

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v2, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    :cond_2
    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInUse:Z

    return-object v0

    :catch_0
    move-exception p1

    if-eqz v0, :cond_3

    iget-boolean v0, v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z

    if-nez v0, :cond_4

    :cond_3
    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v0, v1, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeFinalizeStatement(JJ)V

    :cond_4
    throw p1
.end method

.method private static native nativeBindBlob(JJI[B)V
.end method

.method private static native nativeBindDouble(JJID)V
.end method

.method private static native nativeBindLong(JJIJ)V
.end method

.method private static native nativeBindNull(JJI)V
.end method

.method private static native nativeBindString(JJILjava/lang/String;)V
.end method

.method private static native nativeCancel(J)V
.end method

.method private static native nativeClose(J)V
.end method

.method private static native nativeExecute(JJ)V
.end method

.method private static native nativeExecuteForBlobFileDescriptor(JJ)I
.end method

.method private static native nativeExecuteForChangedRowCount(JJ)I
.end method

.method private static native nativeExecuteForCursorWindow(JJLandroid/database/CursorWindow;IIZ)J
.end method

.method private static native nativeExecuteForLastInsertedRowId(JJ)J
.end method

.method private static native nativeExecuteForLong(JJ)J
.end method

.method private static native nativeExecuteForString(JJ)Ljava/lang/String;
.end method

.method private static native nativeFinalizeStatement(JJ)V
.end method

.method private static native nativeGetColumnCount(JJ)I
.end method

.method private static native nativeGetColumnName(JJI)Ljava/lang/String;
.end method

.method private static native nativeGetDbLookaside(J)I
.end method

.method private static native nativeGetParameterCount(JJ)I
.end method

.method private static native nativeHasCodec()Z
.end method

.method private static native nativeIsReadOnly(JJ)Z
.end method

.method private static native nativeOpen(Ljava/lang/String;ILjava/lang/String;ZZ)J
.end method

.method private static native nativePrepareStatement(JLjava/lang/String;)J
.end method

.method private static native nativeRegisterCustomFunction(JLorg/sqlite/database/sqlite/SQLiteCustomFunction;)V
.end method

.method private static native nativeRegisterLocalizedCollators(JLjava/lang/String;)V
.end method

.method private static native nativeResetCancel(JZ)V
.end method

.method private static native nativeResetStatementAndClearBindings(JJ)V
.end method

.method private vVTkjP()V
    .locals 6

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqccBR:Z

    if-nez v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-boolean v0, v0, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    if-eqz v0, :cond_0

    const-wide/16 v0, 0x1

    goto :goto_0

    :cond_0
    const-wide/16 v0, 0x0

    :goto_0
    const-string v2, "KYhnaj\u000eKGmoIEIyNU^A"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x0

    invoke-virtual {p0, v2, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v4

    cmp-long v2, v4, v0

    if-eqz v2, :cond_1

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "KYhnaj\u000eKGmoIEIyNU^A\u0014"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v0, v1}, Ljava/lang/StringBuilder;->append(J)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p0, v0, v3, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    :cond_1
    return-void
.end method

.method private xCJBFN(Landroid/os/CancellationSignal;)V
    .locals 2

    if-eqz p1, :cond_2

    sget-boolean v0, Lorg/sqlite/database/sqlite/SQLiteConnection;->ltsFhd:Z

    if-nez v0, :cond_1

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqxFOI:I

    if-lez v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_1
    :goto_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqxFOI:I

    add-int/lit8 v0, v0, -0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YqxFOI:I

    if-nez v0, :cond_2

    const/4 v0, 0x0

    invoke-virtual {p1, v0}, Landroid/os/CancellationSignal;->setOnCancelListener(Landroid/os/CancellationSignal$OnCancelListener;)V

    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    const/4 p1, 0x0

    invoke-static {v0, v1, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeResetCancel(JZ)V

    :cond_2
    return-void
.end method

.method private xUYWJB(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 4

    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v2, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v0, v1, v2, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeFinalizeStatement(JJ)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->eZKvwk(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    return-void
.end method


# virtual methods
.method close()V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->eUeiDD(Z)V

    return-void
.end method

.method collectDbStats(Ljava/util/ArrayList;)V
    .locals 26
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;",
            ">;)V"
        }
    .end annotation

    move-object/from16 v9, p0

    move-object/from16 v10, p1

    const-string v11, "KYhnaj\u000e"

    iget-wide v0, v9, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeGetDbLookaside(J)I

    move-result v2

    const/4 v12, 0x0

    const-wide/16 v13, 0x0

    :try_start_0
    const-string v0, "KYhnaj\u000e]Ixo\u007fAHSKD\u001c"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v9, v0, v12, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v3
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_1

    :try_start_1
    const-string v0, "KYhnaj\u000e]Ixo\u007fQN\\@\u000b"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v9, v0, v12, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v0
    :try_end_1
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_1 .. :try_end_1} :catch_0

    move-wide v5, v0

    goto :goto_1

    :catch_0
    move-exception v0

    goto :goto_0

    :catch_1
    move-exception v0

    move-wide v3, v13

    :goto_0
    move-wide v5, v13

    :goto_1
    move-object/from16 v1, p0

    invoke-direct/range {v1 .. v6}, Lorg/sqlite/database/sqlite/SQLiteConnection;->iVVBrW(IJJ)Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;

    move-result-object v0

    invoke-virtual {v10, v0}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    new-instance v15, Landroid/database/CursorWindow;

    const-string v0, "xdEEIHZiJL~AVT"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {v15, v0}, Landroid/database/CursorWindow;-><init>(Ljava/lang/String;)V

    :try_start_2
    const-string v0, "KYhnaj\u000eIIkkBCTCz\\NA]\u0007"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    const/4 v3, 0x0

    const/4 v5, 0x0

    const/4 v6, 0x0

    const/4 v7, 0x0

    const/4 v8, 0x0

    move-object/from16 v1, p0

    move-object v4, v15

    invoke-virtual/range {v1 .. v8}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForCursorWindow(Ljava/lang/String;[Ljava/lang/Object;Landroid/database/CursorWindow;IIZLandroid/os/CancellationSignal;)I

    const/4 v1, 0x1

    move v2, v1

    :goto_2
    invoke-virtual {v15}, Landroid/database/CursorWindow;->getNumRows()I

    move-result v0

    if-ge v2, v0, :cond_1

    invoke-virtual {v15, v2, v1}, Landroid/database/CursorWindow;->getString(II)Ljava/lang/String;

    move-result-object v3

    const/4 v0, 0x2

    invoke-virtual {v15, v2, v0}, Landroid/database/CursorWindow;->getString(II)Ljava/lang/String;

    move-result-object v4
    :try_end_2
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_2 .. :try_end_2} :catch_4
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static {v11}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v5, "5{HNItMB]q~\u001b"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v9, v0, v12, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v5
    :try_end_3
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_3 .. :try_end_3} :catch_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_0

    :try_start_4
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static {v11}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v0, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v7, "5{HNIt]DRz1"

    invoke-static {v7}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v0, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v9, v0, v12, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide v7
    :try_end_4
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_4 .. :try_end_4} :catch_2
    .catchall {:try_start_4 .. :try_end_4} :catchall_0

    move-wide/from16 v18, v5

    move-wide/from16 v20, v7

    goto :goto_4

    :catch_2
    move-exception v0

    goto :goto_3

    :catch_3
    move-exception v0

    move-wide v5, v13

    :goto_3
    move-wide/from16 v18, v5

    move-wide/from16 v20, v13

    :goto_4
    :try_start_5
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v5, ";+\u0001HX_ON@zn\t\u0002"

    invoke-static {v5}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v0, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v4}, Ljava/lang/String;->isEmpty()Z

    move-result v3

    if-nez v3, :cond_0

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-virtual {v3, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v3, "!+"

    invoke-static {v3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v0, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    :cond_0
    move-object/from16 v17, v0

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;

    const/16 v22, 0x0

    const/16 v23, 0x0

    const/16 v24, 0x0

    const/16 v25, 0x0

    move-object/from16 v16, v0

    invoke-direct/range {v16 .. v25}, Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;-><init>(Ljava/lang/String;JJIIII)V

    invoke-virtual {v10, v0}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z
    :try_end_5
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_5 .. :try_end_5} :catch_4
    .catchall {:try_start_5 .. :try_end_5} :catchall_0

    add-int/lit8 v2, v2, 0x1

    goto/16 :goto_2

    :catchall_0
    move-exception v0

    invoke-virtual {v15}, Landroid/database/CursorWindow;->close()V

    throw v0

    :catch_4
    move-exception v0

    :cond_1
    invoke-virtual {v15}, Landroid/database/CursorWindow;->close()V

    return-void
.end method

.method collectDbStatsUnsafe(Ljava/util/ArrayList;)V
    .locals 6
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Ljava/util/ArrayList<",
            "Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;",
            ">;)V"
        }
    .end annotation

    const/4 v1, 0x0

    const-wide/16 v2, 0x0

    const-wide/16 v4, 0x0

    move-object v0, p0

    invoke-direct/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteConnection;->iVVBrW(IJJ)Lorg/sqlite/database/sqlite/SQLiteDebug$DbStats;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/util/ArrayList;->add(Ljava/lang/Object;)Z

    return-void
.end method

.method describeCurrentOperationUnsafe()Ljava/lang/String;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->describeCurrentOperation()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method

.method public dump(Landroid/util/Printer;Z)V
    .locals 0

    invoke-virtual {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->dumpUnsafe(Landroid/util/Printer;Z)V

    return-void
.end method

.method dumpUnsafe(Landroid/util/Printer;Z)V
    .locals 3

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "XdGGIHZDGq*\u0003"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xijmDw:I

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ":"

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    if-eqz p2, :cond_0

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, ";+JFBEKN\\veNrST\u001f\u0010\u0017J"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v1, v2}, Ljava/lang/Long;->toHexString(J)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_0
    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, ";+@Z|YG@ImscMIH@SS[FR\u0001\u001e"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YnHMQY:Z

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Z)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, ";+FG@RoADp}rGFBj^KKfL^L\\LFU_G\t\u0016"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->MECQhq:Z

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Z)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->dump(Landroid/util/Printer;Z)V

    if-eqz p2, :cond_1

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {p2, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->dump(Landroid/util/Printer;)V

    :cond_1
    return-void
.end method

.method public enableLocalizedCollators()V
    .locals 1

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeHasCodec()Z

    move-result v0

    if-eqz v0, :cond_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->MPPfhV()V

    :cond_0
    return-void
.end method

.method public execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V
    .locals 5

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "~sLJY_K"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecute(JJ)V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-void

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_0

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForBlobFileDescriptor(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Landroid/os/ParcelFileDescriptor;
    .locals 5

    if-eqz p1, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "~sLJY_KkGmHLME`L\\BvLOXLTH[UC"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForBlobFileDescriptor(JJ)I

    move-result p2

    if-ltz p2, :cond_0

    invoke-static {p2}, Landroid/os/ParcelFileDescriptor;->adoptFd(I)Landroid/os/ParcelFileDescriptor;

    move-result-object p2
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    goto :goto_0

    :cond_0
    const/4 p2, 0x0

    :goto_0
    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-object p2

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_1

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_1
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForChangedRowCount(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)I
    .locals 7

    const-string v0, "xcHGKNJ\u007fGhy\u001d"

    if-eqz p1, :cond_2

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v2, "~sLJY_KkGmIHCIA@Tu]^\u007fTKSL"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v1

    const/4 v2, 0x0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v5, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v3, v4, v5, v6}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForChangedRowCount(JJ)I

    move-result v2
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperationDeferLog(I)Z

    move-result p1

    if-eqz p1, :cond_0

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    new-instance p2, Ljava/lang/StringBuilder;

    invoke-direct {p2}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-virtual {p1, v1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->logOperation(ILjava/lang/String;)V

    :cond_0
    return v2

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_0

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v1, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperationDeferLog(I)Z

    move-result p2

    if-eqz p2, :cond_1

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    new-instance p3, Ljava/lang/StringBuilder;

    invoke-direct {p3}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p3, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p3

    invoke-virtual {p3, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p3

    invoke-virtual {p3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, v1, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->logOperation(ILjava/lang/String;)V

    :cond_1
    throw p1

    :cond_2
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForCursorWindow(Ljava/lang/String;[Ljava/lang/Object;Landroid/database/CursorWindow;IIZLandroid/os/CancellationSignal;)I
    .locals 21

    move-object/from16 v1, p0

    move-object/from16 v0, p1

    move-object/from16 v2, p2

    move-object/from16 v10, p3

    move/from16 v11, p4

    move-object/from16 v12, p7

    const-string v13, "7+JFYEZHLMeWQ\u001a"

    const-string v14, "7+O@@GKIzp}S\u001f"

    const-string v15, "7+HJX^OAxpy\u001d"

    const-string v16, "<\'\tZXJ\\Yxpy\u001d"

    const-string v17, "lbGMC\\\u0013\n"

    if-eqz v0, :cond_3

    if-eqz v10, :cond_2

    invoke-virtual/range {p3 .. p3}, Landroid/database/CursorWindow;->acquireReference()V

    :try_start_0
    iget-object v3, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v4, "~sLJY_KkGmIUPTIWgN\\MSL"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4, v0, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v9
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_a

    const/16 v18, -0x1

    :try_start_1
    invoke-direct/range {p0 .. p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object v8
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_2
    .catchall {:try_start_1 .. :try_end_1} :catchall_8

    :try_start_2
    invoke-direct {v1, v8}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {v1, v8, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {v1, v8}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {v1, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_7

    :try_start_3
    iget-wide v2, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v4, v8, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_5

    move-object/from16 v6, p3

    move/from16 v7, p4

    move-object/from16 v19, v13

    move-object v13, v8

    move/from16 v8, p5

    move-object/from16 v20, v14

    move v14, v9

    move/from16 v9, p6

    :try_start_4
    invoke-static/range {v2 .. v9}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForCursorWindow(JJLandroid/database/CursorWindow;IIZ)J

    move-result-wide v2
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_4

    const/16 v0, 0x20

    shr-long v4, v2, v0

    long-to-int v4, v4

    long-to-int v2, v2

    :try_start_5
    invoke-virtual/range {p3 .. p3}, Landroid/database/CursorWindow;->getNumRows()I

    move-result v3
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_3

    :try_start_6
    invoke-virtual {v10, v4}, Landroid/database/CursorWindow;->setStartPosition(I)V
    :try_end_6
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :try_start_7
    invoke-direct {v1, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_1

    :try_start_8
    invoke-direct {v1, v13}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_8
    .catch Ljava/lang/RuntimeException; {:try_start_8 .. :try_end_8} :catch_0
    .catchall {:try_start_8 .. :try_end_8} :catchall_0

    :try_start_9
    iget-object v0, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v0, v14}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperationDeferLog(I)Z

    move-result v0

    if-eqz v0, :cond_0

    iget-object v0, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    new-instance v5, Ljava/lang/StringBuilder;

    invoke-direct {v5}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static/range {v17 .. v17}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-virtual {v5, v10}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-static/range {v16 .. v16}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-virtual {v5, v11}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-static {v15}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v5, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v5

    invoke-virtual {v5, v4}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-static/range {v20 .. v20}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-virtual {v4, v3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-static/range {v19 .. v19}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-virtual {v0, v14, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->logOperation(ILjava/lang/String;)V
    :try_end_9
    .catchall {:try_start_9 .. :try_end_9} :catchall_a

    :cond_0
    invoke-virtual/range {p3 .. p3}, Landroid/database/CursorWindow;->releaseReference()V

    return v2

    :catchall_0
    move-exception v0

    goto/16 :goto_5

    :catch_0
    move-exception v0

    move/from16 v18, v4

    goto :goto_4

    :catchall_1
    move-exception v0

    move/from16 v18, v4

    goto :goto_3

    :catchall_2
    move-exception v0

    goto :goto_0

    :catchall_3
    move-exception v0

    move/from16 v3, v18

    :goto_0
    move/from16 v18, v4

    goto :goto_2

    :catchall_4
    move-exception v0

    goto :goto_1

    :catchall_5
    move-exception v0

    move-object/from16 v19, v13

    move-object/from16 v20, v14

    move-object v13, v8

    move v14, v9

    :goto_1
    move/from16 v2, v18

    move v3, v2

    :goto_2
    :try_start_a
    invoke-direct {v1, v12}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw v0
    :try_end_a
    .catchall {:try_start_a .. :try_end_a} :catchall_6

    :catchall_6
    move-exception v0

    goto :goto_3

    :catchall_7
    move-exception v0

    move-object/from16 v19, v13

    move-object/from16 v20, v14

    move-object v13, v8

    move v14, v9

    move/from16 v2, v18

    move v3, v2

    :goto_3
    :try_start_b
    invoke-direct {v1, v13}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw v0
    :try_end_b
    .catch Ljava/lang/RuntimeException; {:try_start_b .. :try_end_b} :catch_1
    .catchall {:try_start_b .. :try_end_b} :catchall_9

    :catch_1
    move-exception v0

    goto :goto_4

    :catchall_8
    move-exception v0

    move-object/from16 v19, v13

    move-object/from16 v20, v14

    move v14, v9

    move/from16 v2, v18

    move v3, v2

    move v4, v3

    goto :goto_5

    :catch_2
    move-exception v0

    move-object/from16 v19, v13

    move-object/from16 v20, v14

    move v14, v9

    move/from16 v2, v18

    move v3, v2

    :goto_4
    :try_start_c
    iget-object v4, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v4, v14, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw v0
    :try_end_c
    .catchall {:try_start_c .. :try_end_c} :catchall_9

    :catchall_9
    move-exception v0

    move/from16 v4, v18

    :goto_5
    :try_start_d
    iget-object v5, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {v5, v14}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperationDeferLog(I)Z

    move-result v5

    if-eqz v5, :cond_1

    iget-object v5, v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    new-instance v6, Ljava/lang/StringBuilder;

    invoke-direct {v6}, Ljava/lang/StringBuilder;-><init>()V

    invoke-static/range {v17 .. v17}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6, v10}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-static/range {v16 .. v16}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6, v11}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-static {v15}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6, v4}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-static/range {v20 .. v20}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v6

    invoke-virtual {v4, v6}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-virtual {v4, v3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-static/range {v19 .. v19}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v5, v14, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->logOperation(ILjava/lang/String;)V

    :cond_1
    throw v0
    :try_end_d
    .catchall {:try_start_d .. :try_end_d} :catchall_a

    :catchall_a
    move-exception v0

    invoke-virtual/range {p3 .. p3}, Landroid/database/CursorWindow;->releaseReference()V

    throw v0

    :cond_2
    new-instance v0, Ljava/lang/IllegalArgumentException;

    const-string v2, "lbGMC\\\u000e@]l~\u0000LHR\u0005RB\u0012GIWR\u0013"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v0, v2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw v0

    :cond_3
    new-instance v0, Ljava/lang/IllegalArgumentException;

    const-string v2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v0, v2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method public executeForLastInsertedRowId(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J
    .locals 5

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "~sLJY_KkGmFAQSoKCB@]Y_lROf^"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForLastInsertedRowId(JJ)J

    move-result-wide v1
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-wide v1

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_0

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J
    .locals 5

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "~sLJY_KkGmFOL@"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForLong(JJ)J

    move-result-wide v1
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-wide v1

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_0

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;
    .locals 5

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "~sLJY_KkGmYTPNHB"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_2

    :try_start_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->TcIlUH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BdIwmZ(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;[Ljava/lang/Object;)V

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->YRfHhH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BTEnBb(Landroid/os/CancellationSignal;)V
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    :try_start_2
    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeExecuteForString(JJ)Ljava/lang/String;

    move-result-object p2
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    :try_start_3
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :try_start_4
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_4
    .catch Ljava/lang/RuntimeException; {:try_start_4 .. :try_end_4} :catch_0
    .catchall {:try_start_4 .. :try_end_4} :catchall_2

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-object p2

    :catchall_0
    move-exception p2

    :try_start_5
    invoke-direct {p0, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->xCJBFN(Landroid/os/CancellationSignal;)V

    throw p2
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    :catchall_1
    move-exception p2

    :try_start_6
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2
    :try_end_6
    .catch Ljava/lang/RuntimeException; {:try_start_6 .. :try_end_6} :catch_0
    .catchall {:try_start_6 .. :try_end_6} :catchall_2

    :catchall_2
    move-exception p1

    goto :goto_0

    :catch_0
    move-exception p1

    :try_start_7
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_7
    .catchall {:try_start_7 .. :try_end_7} :catchall_2

    :goto_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method protected finalize()V
    .locals 5

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->BzDBWC:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    if-eqz v0, :cond_0

    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    const-wide/16 v3, 0x0

    cmp-long v1, v1, v3

    if-eqz v1, :cond_0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->onConnectionLeaked()V

    :cond_0
    const/4 v0, 0x1

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->eUeiDD(Z)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    return-void

    :catchall_0
    move-exception v0

    invoke-super {p0}, Ljava/lang/Object;->finalize()V

    throw v0
.end method

.method public getConnectionId()I
    .locals 1

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xijmDw:I

    return v0
.end method

.method isPreparedStatementInCache(Ljava/lang/String;)Z
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->CIIcVm:Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object p1

    if-eqz p1, :cond_0

    const/4 p1, 0x1

    goto :goto_0

    :cond_0
    const/4 p1, 0x0

    :goto_0
    return p1
.end method

.method public isPrimaryConnection()Z
    .locals 1

    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->YnHMQY:Z

    return v0
.end method

.method public onCancel()V
    .locals 2

    iget-wide v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeCancel(J)V

    return-void
.end method

.method public prepare(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteStatementInfo;)V
    .locals 8

    if-eqz p1, :cond_2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    const-string v1, "kyLYMYK"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    const/4 v2, 0x0

    invoke-virtual {v0, v1, p1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->beginOperation(Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)I

    move-result v0

    :try_start_0
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->mjMqcU(Ljava/lang/String;)Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    if-eqz p2, :cond_1

    :try_start_1
    iget v1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mNumParameters:I

    iput v1, p2, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->numParameters:I

    iget-boolean v1, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mReadOnly:Z

    iput-boolean v1, p2, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->readOnly:Z

    iget-wide v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v3, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeGetColumnCount(JJ)I

    move-result v1

    if-nez v1, :cond_0

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteConnection;->THOCGY:[Ljava/lang/String;

    iput-object v1, p2, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->columnNames:[Ljava/lang/String;

    goto :goto_1

    :cond_0
    new-array v2, v1, [Ljava/lang/String;

    iput-object v2, p2, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->columnNames:[Ljava/lang/String;

    const/4 v2, 0x0

    :goto_0
    if-ge v2, v1, :cond_1

    iget-object v3, p2, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->columnNames:[Ljava/lang/String;

    iget-wide v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    iget-wide v6, p1, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v4, v5, v6, v7, v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeGetColumnName(JJI)Ljava/lang/String;

    move-result-object v4

    aput-object v4, v3, v2
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    add-int/lit8 v2, v2, 0x1

    goto :goto_0

    :catchall_0
    move-exception p2

    :try_start_2
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    throw p2

    :cond_1
    :goto_1
    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->QjscXH(Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    :try_end_2
    .catch Ljava/lang/RuntimeException; {:try_start_2 .. :try_end_2} :catch_0
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p1, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    return-void

    :catchall_1
    move-exception p1

    goto :goto_2

    :catch_0
    move-exception p1

    :try_start_3
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->failOperation(ILjava/lang/Exception;)V

    throw p1
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    :goto_2
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->TdXmMq:Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;

    invoke-virtual {p2, v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$OperationLog;->endOperation(I)V

    throw p1

    :cond_2
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method reconfigure(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V
    .locals 6

    const/4 v0, 0x0

    iput-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->MECQhq:Z

    iget-object v1, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {v1}, Ljava/util/ArrayList;->size()I

    move-result v1

    move v2, v0

    :goto_0
    if-ge v2, v1, :cond_1

    iget-object v3, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {v3, v2}, Ljava/util/ArrayList;->get(I)Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Lorg/sqlite/database/sqlite/SQLiteCustomFunction;

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v4, v4, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->customFunctions:Ljava/util/ArrayList;

    invoke-virtual {v4, v3}, Ljava/util/ArrayList;->contains(Ljava/lang/Object;)Z

    move-result v4

    if-nez v4, :cond_0

    iget-wide v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->QIHKJR:J

    invoke-static {v4, v5, v3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->nativeRegisterCustomFunction(JLorg/sqlite/database/sqlite/SQLiteCustomFunction;)V

    :cond_0
    add-int/lit8 v2, v2, 0x1

    goto :goto_0

    :cond_1
    iget-boolean v1, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-boolean v2, v2, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->foreignKeyConstraintsEnabled:Z

    const/4 v3, 0x1

    if-eq v1, v2, :cond_2

    move v1, v3

    goto :goto_1

    :cond_2
    move v1, v0

    :goto_1
    iget v2, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget v4, v4, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->openFlags:I

    xor-int/2addr v2, v4

    const/high16 v4, 0x20000000

    and-int/2addr v2, v4

    if-eqz v2, :cond_3

    move v0, v3

    :cond_3
    iget-object v2, p1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v4, v4, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->locale:Ljava/util/Locale;

    invoke-virtual {v2, v4}, Ljava/util/Locale;->equals(Ljava/lang/Object;)Z

    move-result v2

    xor-int/2addr v2, v3

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    invoke-virtual {v3, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->updateParametersFrom(Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;)V

    if-eqz v1, :cond_4

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->vVTkjP()V

    :cond_4
    if-eqz v0, :cond_5

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->BkUSlK()V

    :cond_5
    if-eqz v2, :cond_6

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection;->MPPfhV()V

    :cond_6
    return-void
.end method

.method setOnlyAllowReadOnlyOperations(Z)V
    .locals 0

    iput-boolean p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->MECQhq:Z

    return-void
.end method

.method public toString()Ljava/lang/String;
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "HZe@XNmBFqoCVNIK\n\u0007"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xvgdZC:Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteDatabaseConfiguration;->path:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ";#"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection;->xijmDw:I

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v0

    const-string v1, ")"

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method
