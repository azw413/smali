.class public abstract Lorg/sqlite/database/sqlite/SQLiteProgram;
.super Lorg/sqlite/database/sqlite/SQLiteClosable;


# static fields
.field private static final THOCGY:[Ljava/lang/String;


# instance fields
.field private final CxetlJ:[Ljava/lang/String;

.field private final QYWPxt:Z

.field private final YfireR:I

.field private final YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

.field private final xBlCIA:Ljava/lang/String;

.field private final xUNaEH:[Ljava/lang/Object;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    new-array v0, v0, [Ljava/lang/String;

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteProgram;->THOCGY:[Ljava/lang/String;

    return-void
.end method

.method constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V
    .locals 4

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;-><init>()V

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {p2}, Ljava/lang/String;->trim()Ljava/lang/String;

    move-result-object p2

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xBlCIA:Ljava/lang/String;

    invoke-static {p2}, Landroid/database/DatabaseUtils;->getSqlStatementType(Ljava/lang/String;)I

    move-result v0

    const/4 v1, 0x0

    packed-switch v0, :pswitch_data_0

    const/4 v2, 0x1

    if-ne v0, v2, :cond_0

    goto :goto_0

    :pswitch_0
    iput-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->QYWPxt:Z

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteProgram;->THOCGY:[Ljava/lang/String;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->CxetlJ:[Ljava/lang/String;

    iput v1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    goto :goto_1

    :cond_0
    move v2, v1

    :goto_0
    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;

    invoke-direct {v0}, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;-><init>()V

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v3

    invoke-virtual {p1, v2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadDefaultConnectionFlags(Z)I

    move-result p1

    invoke-virtual {v3, p2, p1, p4, v0}, Lorg/sqlite/database/sqlite/SQLiteSession;->prepare(Ljava/lang/String;ILandroid/os/CancellationSignal;Lorg/sqlite/database/sqlite/SQLiteStatementInfo;)V

    iget-boolean p1, v0, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->readOnly:Z

    iput-boolean p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->QYWPxt:Z

    iget-object p1, v0, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->columnNames:[Ljava/lang/String;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->CxetlJ:[Ljava/lang/String;

    iget p1, v0, Lorg/sqlite/database/sqlite/SQLiteStatementInfo;->numParameters:I

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    :goto_1
    if-eqz p3, :cond_2

    array-length p1, p3

    iget p2, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    if-gt p1, p2, :cond_1

    goto :goto_2

    :cond_1
    new-instance p1, Ljava/lang/IllegalArgumentException;

    new-instance p2, Ljava/lang/StringBuilder;

    invoke-direct {p2}, Ljava/lang/StringBuilder;-><init>()V

    const-string p4, "OdF\tAJ@T\u0008}cNF\u0007GWWR_LROM\u0013\u0018\u000f"

    invoke-static {p4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p4

    invoke-virtual {p2, p4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    array-length p3, p3

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    const-string p3, ";j[NYFKC\\l*WGUC\u0005@U]_U_[Y\u0018MOE\u0014G^P\u0000ETY^jchfJ\u0008oafbv0"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    iget p3, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    const-string p3, ";j[NYFKC\\l$"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_2
    :goto_2
    iget p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    if-eqz p1, :cond_3

    new-array p1, p1, [Ljava/lang/Object;

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xUNaEH:[Ljava/lang/Object;

    if-eqz p3, :cond_4

    array-length p2, p3

    invoke-static {p3, v1, p1, v1, p2}, Ljava/lang/System;->arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V

    goto :goto_3

    :cond_3
    const/4 p1, 0x0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xUNaEH:[Ljava/lang/Object;

    :cond_4
    :goto_3
    return-void

    :pswitch_data_0
    .packed-switch 0x4
        :pswitch_0
        :pswitch_0
        :pswitch_0
    .end packed-switch
.end method

.method private vdEtce(ILjava/lang/Object;)V
    .locals 2

    const/4 v0, 0x1

    if-lt p1, v0, :cond_0

    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    if-gt p1, v1, :cond_0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xUNaEH:[Ljava/lang/Object;

    sub-int/2addr p1, v0

    aput-object p2, v1, p1

    return-void

    :cond_0
    new-instance p2, Ljava/lang/IllegalArgumentException;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "XjGGC_\u000eOAqn\u0000CUAP]B\\]\u001cZJ\u001dQA^TL\u0013"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";iLJM^]H\u0008kbE\u0002NHAU_\u0012@O\u001bQHL\u000fUW\u0014AW[GS\u000e\u0018\n[fh(M\\`pfk`~s2a}h>"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YfireR:I

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";{H[MFKYMmy\u000e"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p2, p1}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p2
.end method


# virtual methods
.method public bindAllArgsAsStrings([Ljava/lang/String;)V
    .locals 2

    if-eqz p1, :cond_0

    array-length v0, p1

    :goto_0
    if-eqz v0, :cond_0

    add-int/lit8 v1, v0, -0x1

    aget-object v1, p1, v1

    invoke-virtual {p0, v0, v1}, Lorg/sqlite/database/sqlite/SQLiteProgram;->bindString(ILjava/lang/String;)V

    add-int/lit8 v0, v0, -0x1

    goto :goto_0

    :cond_0
    return-void
.end method

.method public bindBlob(I[B)V
    .locals 2

    if-eqz p2, :cond_0

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteProgram;->vdEtce(ILjava/lang/Object;)V

    return-void

    :cond_0
    new-instance p2, Ljava/lang/IllegalArgumentException;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "ocL\tNB@I\u0008ikLWB\u0006DD\u0007[GX^F\u001d"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";bZ\tB^BA"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p2, p1}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p2
.end method

.method public bindDouble(ID)V
    .locals 0

    invoke-static {p2, p3}, Ljava/lang/Double;->valueOf(D)Ljava/lang/Double;

    move-result-object p2

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteProgram;->vdEtce(ILjava/lang/Object;)V

    return-void
.end method

.method public bindLong(IJ)V
    .locals 0

    invoke-static {p2, p3}, Ljava/lang/Long;->valueOf(J)Ljava/lang/Long;

    move-result-object p2

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteProgram;->vdEtce(ILjava/lang/Object;)V

    return-void
.end method

.method public bindNull(I)V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, p1, v0}, Lorg/sqlite/database/sqlite/SQLiteProgram;->vdEtce(ILjava/lang/Object;)V

    return-void
.end method

.method public bindString(ILjava/lang/String;)V
    .locals 2

    if-eqz p2, :cond_0

    invoke-direct {p0, p1, p2}, Lorg/sqlite/database/sqlite/SQLiteProgram;->vdEtce(ILjava/lang/Object;)V

    return-void

    :cond_0
    new-instance p2, Ljava/lang/IllegalArgumentException;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "ocL\tNB@I\u0008ikLWB\u0006DD\u0007[GX^F\u001d"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p1

    const-string v0, ";bZ\tB^BA"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p1, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p2, p1}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p2
.end method

.method public clearBindings()V
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xUNaEH:[Ljava/lang/Object;

    if-eqz v0, :cond_0

    const/4 v1, 0x0

    invoke-static {v0, v1}, Ljava/util/Arrays;->fill([Ljava/lang/Object;Ljava/lang/Object;)V

    :cond_0
    return-void
.end method

.method final getBindArgs()[Ljava/lang/Object;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xUNaEH:[Ljava/lang/Object;

    return-object v0
.end method

.method final getColumnNames()[Ljava/lang/String;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->CxetlJ:[Ljava/lang/String;

    return-object v0
.end method

.method protected final getConnectionFlags()I
    .locals 2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iget-boolean v1, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->QYWPxt:Z

    invoke-virtual {v0, v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadDefaultConnectionFlags(Z)I

    move-result v0

    return v0
.end method

.method final getDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    return-object v0
.end method

.method protected final getSession()Lorg/sqlite/database/sqlite/SQLiteSession;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getThreadSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    return-object v0
.end method

.method final getSql()Ljava/lang/String;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->xBlCIA:Ljava/lang/String;

    return-object v0
.end method

.method public final getUniqueId()I
    .locals 1
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    const/4 v0, -0x1

    return v0
.end method

.method protected onAllReferencesReleased()V
    .locals 0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteProgram;->clearBindings()V

    return-void
.end method

.method protected final onCorruption()V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteProgram;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->onCorruption()V

    return-void
.end method
