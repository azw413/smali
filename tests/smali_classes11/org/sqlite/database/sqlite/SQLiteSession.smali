.class public final Lorg/sqlite/database/sqlite/SQLiteSession;
.super Ljava/lang/Object;


# annotations
.annotation system Ldalvik/annotation/MemberClasses;
    value = {
        Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;
    }
.end annotation


# static fields
.field public static final TRANSACTION_MODE_DEFERRED:I = 0x0

.field public static final TRANSACTION_MODE_EXCLUSIVE:I = 0x2

.field public static final TRANSACTION_MODE_IMMEDIATE:I = 0x1

.field static final synthetic ltsFhd:Z


# instance fields
.field private CUWACC:I

.field private final EGwcnA:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

.field private TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

.field private edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

.field private mZwSQH:I

.field private vTTtXg:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x1

    sput-boolean v0, Lorg/sqlite/database/sqlite/SQLiteSession;->ltsFhd:Z

    return-void
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;)V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    if-eqz p1, :cond_0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->EGwcnA:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    return-void

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string v0, "xdGGIHZDGqZOMK\u0006HETF\tRTJ\u001dZJ\u001a_A_Z"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method private BSpGVa(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;)Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->vTTtXg:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    iget-object v2, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->vTTtXg:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    const/4 v1, 0x0

    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMarkedSuccessful:Z

    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mChildFailed:Z

    goto :goto_0

    :cond_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    invoke-direct {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;-><init>(Lorg/sqlite/database/sqlite/SQLiteSession$1;)V

    :goto_0
    iput p1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMode:I

    iput-object p2, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mListener:Lorg/sqlite/database/sqlite/SQLiteTransactionListener;

    return-object v0
.end method

.method private EWcGlH(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V
    .locals 2

    if-eqz p4, :cond_0

    invoke-virtual {p4}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    const/4 v1, 0x0

    if-nez v0, :cond_1

    invoke-direct {p0, v1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :cond_1
    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-nez p3, :cond_2

    packed-switch p1, :pswitch_data_0

    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    goto :goto_1

    :pswitch_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string v0, "YNn`b\u000bkukS_skqc\u001e"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    :goto_0
    invoke-virtual {p3, v0, v1, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    goto :goto_2

    :pswitch_1
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string v0, "YNn`b\u000bg`eZNicsc\u001e"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    goto :goto_0

    :goto_1
    const-string v0, "YNn`b\u0010"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    goto :goto_0

    :cond_2
    :goto_2
    if-eqz p2, :cond_4

    :try_start_1
    invoke-interface {p2}, Lorg/sqlite/database/sqlite/SQLiteTransactionListener;->onBegin()V
    :try_end_1
    .catch Ljava/lang/RuntimeException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    goto :goto_3

    :catch_0
    move-exception p1

    :try_start_2
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-nez p2, :cond_3

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string p3, "IDeenjmf\u0013"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, p3, v1, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    :cond_3
    throw p1

    :cond_4
    :goto_3
    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteSession;->BSpGVa(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;)Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    move-result-object p1

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object p2, p1, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    if-nez p1, :cond_5

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    :cond_5
    return-void

    :catchall_0
    move-exception p1

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-nez p2, :cond_6

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    :cond_6
    throw p1

    nop

    :pswitch_data_0
    .packed-switch 0x1
        :pswitch_1
        :pswitch_0
    .end packed-switch
.end method

.method private MNJaxX()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_1

    iget-boolean v0, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMarkedSuccessful:Z

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/zem\u001e\\semudss{fr;v|k/{}fvwqx7a|hE\u000fCJVBGC\u0008VSPOgZNZHR\u0015\u0014\u0019f_]\u0015YMWR*{xt@l$pmr(fg},Ee?Nbi;}j2tyyz$EtoeOpP~IAWCFF\u000f\u001f\u001b"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0

    :cond_1
    :goto_0
    return-void
.end method

.method private RKfuZ(Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->vTTtXg:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    const/4 v0, 0x0

    iput-object v0, p1, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mListener:Lorg/sqlite/database/sqlite/SQLiteTransactionListener;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->vTTtXg:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    return-void
.end method

.method private TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z
    .locals 1

    if-eqz p4, :cond_0

    invoke-virtual {p4}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_0
    invoke-static {p1}, Landroid/database/DatabaseUtils;->getSqlStatementType(Ljava/lang/String;)I

    move-result p1

    const/4 p2, 0x1

    packed-switch p1, :pswitch_data_0

    const/4 p1, 0x0

    return p1

    :pswitch_0
    invoke-virtual {p0, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->endTransaction(Landroid/os/CancellationSignal;)V

    return p2

    :pswitch_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->setTransactionSuccessful()V

    invoke-virtual {p0, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->endTransaction(Landroid/os/CancellationSignal;)V

    return p2

    :pswitch_2
    const/4 p1, 0x2

    const/4 v0, 0x0

    invoke-virtual {p0, p1, v0, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->beginTransaction(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V

    return p2

    :pswitch_data_0
    .packed-switch 0x4
        :pswitch_2
        :pswitch_1
        :pswitch_0
    .end packed-switch
.end method

.method private YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-nez v0, :cond_2

    sget-boolean v0, Lorg/sqlite/database/sqlite/SQLiteSession;->ltsFhd:Z

    if-nez v0, :cond_1

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_1
    :goto_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->EGwcnA:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    invoke-virtual {v0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->acquireConnection(Ljava/lang/String;ILandroid/os/CancellationSignal;)Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-result-object p1

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iput p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->CUWACC:I

    :cond_2
    iget p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    add-int/lit8 p1, p1, 0x1

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    return-void
.end method

.method private lxJlxZ(JLandroid/os/CancellationSignal;)Z
    .locals 6

    if-eqz p3, :cond_0

    invoke-virtual {p3}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->EGwcnA:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->CUWACC:I

    invoke-virtual {v0, v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->shouldYieldConnection(Lorg/sqlite/database/sqlite/SQLiteConnection;I)Z

    move-result v0

    if-nez v0, :cond_1

    const/4 p1, 0x0

    return p1

    :cond_1
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iget v0, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMode:I

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iget-object v1, v1, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mListener:Lorg/sqlite/database/sqlite/SQLiteTransactionListener;

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->CUWACC:I

    const/4 v3, 0x1

    invoke-direct {p0, p3, v3}, Lorg/sqlite/database/sqlite/SQLiteSession;->vPEDIL(Landroid/os/CancellationSignal;Z)V

    const-wide/16 v4, 0x0

    cmp-long v4, p1, v4

    if-lez v4, :cond_2

    :try_start_0
    invoke-static {p1, p2}, Ljava/lang/Thread;->sleep(J)V
    :try_end_0
    .catch Ljava/lang/InterruptedException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception p1

    :cond_2
    :goto_0
    invoke-direct {p0, v0, v1, v2, p3}, Lorg/sqlite/database/sqlite/SQLiteSession;->EWcGlH(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V

    return v3
.end method

.method private mngYwr()V
    .locals 2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->hasNestedTransaction()Z

    move-result v0

    if-nez v0, :cond_0

    return-void

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/o-f[[uag&qbf|z}xjtwa:xg3\u007f{!gqvjYJ]X\n"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method private pImNT()V
    .locals 3

    sget-boolean v0, Lorg/sqlite/database/sqlite/SQLiteSession;->ltsFhd:Z

    if-nez v0, :cond_1

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v1, :cond_0

    goto :goto_0

    :cond_0
    new-instance v0, Ljava/lang/AssertionError;

    invoke-direct {v0}, Ljava/lang/AssertionError;-><init>()V

    throw v0

    :cond_1
    :goto_0
    if-nez v0, :cond_3

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    if-lez v0, :cond_2

    goto :goto_1

    :cond_2
    new-instance v0, Ljava/lang/AssertionError;

    invoke-direct {v0}, Ljava/lang/AssertionError;-><init>()V

    throw v0

    :cond_3
    :goto_1
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    add-int/lit8 v0, v0, -0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->mZwSQH:I

    if-nez v0, :cond_4

    const/4 v0, 0x0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->EGwcnA:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->releaseConnection(Lorg/sqlite/database/sqlite/SQLiteConnection;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    goto :goto_2

    :catchall_0
    move-exception v1

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    throw v1

    :cond_4
    :goto_2
    return-void
.end method

.method private vPEDIL(Landroid/os/CancellationSignal;Z)V
    .locals 5

    if-eqz p1, :cond_0

    invoke-virtual {p1}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iget-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMarkedSuccessful:Z

    const/4 v2, 0x1

    const/4 v3, 0x0

    if-nez v1, :cond_1

    if-eqz p2, :cond_2

    :cond_1
    iget-boolean p2, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mChildFailed:Z

    if-nez p2, :cond_2

    move p2, v2

    goto :goto_0

    :cond_2
    move p2, v3

    :goto_0
    iget-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mListener:Lorg/sqlite/database/sqlite/SQLiteTransactionListener;

    const/4 v4, 0x0

    if-eqz v1, :cond_4

    if-eqz p2, :cond_3

    :try_start_0
    invoke-interface {v1}, Lorg/sqlite/database/sqlite/SQLiteTransactionListener;->onCommit()V

    goto :goto_1

    :cond_3
    invoke-interface {v1}, Lorg/sqlite/database/sqlite/SQLiteTransactionListener;->onRollback()V
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_1

    :catch_0
    move-exception p2

    goto :goto_2

    :cond_4
    :goto_1
    move v3, p2

    move-object p2, v4

    :goto_2
    iget-object v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->RKfuZ(Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;)V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_5

    if-nez v3, :cond_7

    iput-boolean v2, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mChildFailed:Z

    goto :goto_5

    :cond_5
    if-eqz v3, :cond_6

    :try_start_1
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string v1, "XDdde\u007f\u0015"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    :goto_3
    invoke-virtual {v0, v1, v4, p1}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    goto :goto_4

    :cond_6
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    const-string v1, "IDeenjmf\u0013"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    goto :goto_3

    :goto_4
    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    :cond_7
    :goto_5
    if-nez p2, :cond_8

    return-void

    :cond_8
    throw p2

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1
.end method

.method private xjakuz()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_0

    return-void

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "XjGGC_\u000e]MmlOPJ\u0006QXNA\tSK[OY[S^Z\u0013TPCWUKO/zemLM!mp&k\u007f\'q|ni{sl/ncu}etbcjvc\u0005"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
.end method


# virtual methods
.method public beginTransaction(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V
    .locals 0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->MNJaxX()V

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->EWcGlH(ILorg/sqlite/database/sqlite/SQLiteTransactionListener;ILandroid/os/CancellationSignal;)V

    return-void
.end method

.method public endTransaction(Landroid/os/CancellationSignal;)V
    .locals 1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->xjakuz()V

    sget-boolean v0, Lorg/sqlite/database/sqlite/SQLiteSession;->ltsFhd:Z

    if-nez v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v0, :cond_0

    goto :goto_0

    :cond_0
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_1
    :goto_0
    const/4 v0, 0x0

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->vPEDIL(Landroid/os/CancellationSignal;Z)V

    return-void
.end method

.method public execute(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)V
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    return-void

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->execute(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-void

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForBlobFileDescriptor(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Landroid/os/ParcelFileDescriptor;
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    const/4 p1, 0x0

    return-object p1

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForBlobFileDescriptor(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Landroid/os/ParcelFileDescriptor;

    move-result-object p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-object p1

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForChangedRowCount(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)I
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    const/4 p1, 0x0

    return p1

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForChangedRowCount(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)I

    move-result p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return p1

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForCursorWindow(Ljava/lang/String;[Ljava/lang/Object;Landroid/database/CursorWindow;IIZILandroid/os/CancellationSignal;)I
    .locals 10

    move-object v1, p0

    move-object v0, p1

    move/from16 v2, p7

    move-object/from16 v9, p8

    if-eqz v0, :cond_2

    if-eqz p3, :cond_1

    move-object v4, p2

    invoke-direct {p0, p1, p2, v2, v9}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v3

    if-eqz v3, :cond_0

    invoke-virtual {p3}, Landroid/database/CursorWindow;->clear()V

    const/4 v0, 0x0

    return v0

    :cond_0
    invoke-direct {p0, p1, v2, v9}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object v2, v1, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    move-object v3, p1

    move-object v4, p2

    move-object v5, p3

    move v6, p4

    move v7, p5

    move/from16 v8, p6

    move-object/from16 v9, p8

    invoke-virtual/range {v2 .. v9}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForCursorWindow(Ljava/lang/String;[Ljava/lang/Object;Landroid/database/CursorWindow;IIZLandroid/os/CancellationSignal;)I

    move-result v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return v0

    :catchall_0
    move-exception v0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw v0

    :cond_1
    new-instance v0, Ljava/lang/IllegalArgumentException;

    const-string v2, "lbGMC\\\u000e@]l~\u0000LHR\u0005RB\u0012GIWR\u0013"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v0, v2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw v0

    :cond_2
    new-instance v0, Ljava/lang/IllegalArgumentException;

    const-string v2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-direct {v0, v2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw v0
.end method

.method public executeForLastInsertedRowId(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)J
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    const-wide/16 p1, 0x0

    return-wide p1

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLastInsertedRowId(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-wide p1

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForLong(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)J
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    const-wide/16 p1, 0x0

    return-wide p1

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)J

    move-result-wide p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-wide p1

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public executeForString(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Ljava/lang/String;
    .locals 1

    if-eqz p1, :cond_1

    invoke-direct {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->TWGTmN(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Z

    move-result v0

    if-eqz v0, :cond_0

    const/4 p1, 0x0

    return-object p1

    :cond_0
    invoke-direct {p0, p1, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p3, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->executeForString(Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object p1
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-object p1

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public hasConnection()Z
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz v0, :cond_0

    const/4 v0, 0x1

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    return v0
.end method

.method public hasNestedTransaction()Z
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_0

    iget-object v0, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_0

    const/4 v0, 0x1

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    return v0
.end method

.method public hasTransaction()Z
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz v0, :cond_0

    const/4 v0, 0x1

    goto :goto_0

    :cond_0
    const/4 v0, 0x0

    :goto_0
    return v0
.end method

.method public prepare(Ljava/lang/String;ILandroid/os/CancellationSignal;Lorg/sqlite/database/sqlite/SQLiteStatementInfo;)V
    .locals 0

    if-eqz p1, :cond_1

    if-eqz p3, :cond_0

    invoke-virtual {p3}, Landroid/os/CancellationSignal;->throwIfCanceled()V

    :cond_0
    invoke-direct {p0, p1, p2, p3}, Lorg/sqlite/database/sqlite/SQLiteSession;->YRffBH(Ljava/lang/String;ILandroid/os/CancellationSignal;)V

    :try_start_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-virtual {p2, p1, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection;->prepare(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteStatementInfo;)V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    return-void

    :catchall_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->pImNT()V

    throw p1

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    const-string p2, "hzE\tA^]Y\u0008qeT\u0002EC\u0005^R^E\u0012"

    invoke-static {p2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public setTransactionSuccessful()V
    .locals 2

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->xjakuz()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->MNJaxX()V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    const/4 v1, 0x1

    iput-boolean v1, v0, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMarkedSuccessful:Z

    return-void
.end method

.method public yieldTransaction(JZLandroid/os/CancellationSignal;)Z
    .locals 1

    const/4 v0, 0x0

    if-eqz p3, :cond_0

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->xjakuz()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->MNJaxX()V

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteSession;->mngYwr()V

    goto :goto_0

    :cond_0
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz p3, :cond_5

    iget-boolean p3, p3, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mMarkedSuccessful:Z

    if-nez p3, :cond_5

    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iget-object p3, p3, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mParent:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    if-eqz p3, :cond_1

    goto :goto_2

    :cond_1
    :goto_0
    sget-boolean p3, Lorg/sqlite/database/sqlite/SQLiteSession;->ltsFhd:Z

    if-nez p3, :cond_3

    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->TSqdIK:Lorg/sqlite/database/sqlite/SQLiteConnection;

    if-eqz p3, :cond_2

    goto :goto_1

    :cond_2
    new-instance p1, Ljava/lang/AssertionError;

    invoke-direct {p1}, Ljava/lang/AssertionError;-><init>()V

    throw p1

    :cond_3
    :goto_1
    iget-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteSession;->edNrQs:Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;

    iget-boolean p3, p3, Lorg/sqlite/database/sqlite/SQLiteSession$Transaction;->mChildFailed:Z

    if-eqz p3, :cond_4

    return v0

    :cond_4
    invoke-direct {p0, p1, p2, p4}, Lorg/sqlite/database/sqlite/SQLiteSession;->lxJlxZ(JLandroid/os/CancellationSignal;)Z

    move-result p1

    return p1

    :cond_5
    :goto_2
    return v0
.end method
