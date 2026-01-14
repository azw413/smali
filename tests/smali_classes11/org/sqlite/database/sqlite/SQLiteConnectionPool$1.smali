.class Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;
.super Ljava/lang/Object;

# interfaces
.implements Landroid/os/CancellationSignal$OnCancelListener;


# annotations
.annotation system Ldalvik/annotation/EnclosingMethod;
    value = Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->lHbUVm(Ljava/lang/String;ILandroid/os/CancellationSignal;)Lorg/sqlite/database/sqlite/SQLiteConnection;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x0
    name = null
.end annotation


# instance fields
.field final synthetic CIkqTZ:I

.field final synthetic MufRtT:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

.field final synthetic QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;


# direct methods
.method constructor <init>(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;I)V
    .locals 0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->MufRtT:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iput p3, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->CIkqTZ:I

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method


# virtual methods
.method public onCancel()V
    .locals 3

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->access$000(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;)Ljava/lang/Object;

    move-result-object v0

    monitor-enter v0

    :try_start_0
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->MufRtT:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    iget v1, v1, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;->mNonce:I

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->CIkqTZ:I

    if-ne v1, v2, :cond_0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnectionPool;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteConnectionPool$1;->MufRtT:Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;

    invoke-static {v1, v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->access$100(Lorg/sqlite/database/sqlite/SQLiteConnectionPool;Lorg/sqlite/database/sqlite/SQLiteConnectionPool$ConnectionWaiter;)V

    :cond_0
    monitor-exit v0

    return-void

    :catchall_0
    move-exception v1

    monitor-exit v0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v1
.end method
