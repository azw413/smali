.class public Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;
.super Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;
.source "SimpleSubscriberInfo.java"


# instance fields
.field private final methodInfos:[Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;


# direct methods
.method public constructor <init>(Ljava/lang/Class;Z[Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;)V
    .locals 1
    .param p1, "subscriberClass"    # Ljava/lang/Class;
    .param p2, "shouldCheckSuperclass"    # Z
    .param p3, "methodInfos"    # [Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;

    .line 28
    const/4 v0, 0x0

    invoke-direct {p0, p1, v0, p2}, Lorg/greenrobot/eventbus/meta/AbstractSubscriberInfo;-><init>(Ljava/lang/Class;Ljava/lang/Class;Z)V

    .line 29
    iput-object p3, p0, Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;->methodInfos:[Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;

    .line 30
    return-void
.end method


# virtual methods
.method public declared-synchronized getSubscriberMethods()[Lorg/greenrobot/eventbus/SubscriberMethod;
    .locals 10

    monitor-enter p0

    .line 34
    :try_start_0
    iget-object v0, p0, Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;->methodInfos:[Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;

    array-length v0, v0

    .line 35
    .local v0, "length":I
    new-array v1, v0, [Lorg/greenrobot/eventbus/SubscriberMethod;

    .line 36
    .local v1, "methods":[Lorg/greenrobot/eventbus/SubscriberMethod;
    const/4 v2, 0x0

    .local v2, "i":I
    :goto_0
    if-ge v2, v0, :cond_0

    .line 37
    iget-object v3, p0, Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;->methodInfos:[Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;

    aget-object v3, v3, v2

    .line 38
    .local v3, "info":Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;
    iget-object v5, v3, Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;->methodName:Ljava/lang/String;

    iget-object v6, v3, Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;->eventType:Ljava/lang/Class;

    iget-object v7, v3, Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;->threadMode:Lorg/greenrobot/eventbus/ThreadMode;

    iget v8, v3, Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;->priority:I

    iget-boolean v9, v3, Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;->sticky:Z

    move-object v4, p0

    invoke-virtual/range {v4 .. v9}, Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;->createSubscriberMethod(Ljava/lang/String;Ljava/lang/Class;Lorg/greenrobot/eventbus/ThreadMode;IZ)Lorg/greenrobot/eventbus/SubscriberMethod;

    move-result-object v4

    aput-object v4, v1, v2
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    .line 36
    .end local v3    # "info":Lorg/greenrobot/eventbus/meta/SubscriberMethodInfo;
    add-int/lit8 v2, v2, 0x1

    goto :goto_0

    .line 41
    .end local v2    # "i":I
    .end local p0    # "this":Lorg/greenrobot/eventbus/meta/SimpleSubscriberInfo;
    :cond_0
    monitor-exit p0

    return-object v1

    .line 33
    .end local v0    # "length":I
    .end local v1    # "methods":[Lorg/greenrobot/eventbus/SubscriberMethod;
    :catchall_0
    move-exception v0

    monitor-exit p0

    throw v0
.end method
