package org.springframework.data.gemfire.function.config.four;

import org.springframework.data.gemfire.function.annotation.FunctionId;
import org.springframework.data.gemfire.function.annotation.OnServer;

@OnServer
public interface TestFunctionExecution {
	   @FunctionId("f1")
	   public String getString(Object arg1);
}